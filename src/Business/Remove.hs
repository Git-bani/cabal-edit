{-# LANGUAGE OverloadedStrings #-}

module Business.Remove (removeDependency) where

import Core.Types
import Core.Parser
import Core.Serializer
import Core.Safety
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM)

import Utils.Diff (diffLines, colorizeDiff)

removeDependency :: RemoveOptions -> FilePath -> IO (Result ())
removeDependency opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  
  -- 1. Parse cabal file
  parseResult <- parseCabalFile path
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      -- 2. Process each package
      let eol = cfLineEndings cabalFile
      let initialContent = cfRawContent cabalFile
      
      finalContentResult <- foldM (processPackageRemove opts eol leadingComma cabalFile) (Success initialContent) (roPackageNames opts)
      
      case finalContentResult of
        Failure err -> return $ Failure err
        Success finalContent -> 
          if roDryRun opts
            then do
              logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
              let diffs = diffLines (T.lines initialContent) (T.lines finalContent)
              colorizeDiff diffs
              return $ Success ()
            else safeWriteCabal path finalContent

processPackageRemove :: RemoveOptions -> Text -> Bool -> CabalFile -> Result Text -> Text -> IO (Result Text)
processPackageRemove _ _ _ _ (Failure err) _ = return $ Failure err
processPackageRemove opts eol leadingComma cabalFile (Success currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Failure $ Error err InvalidDependency
    Right pkgName -> do
      let dep = Dependency { depName = pkgName, depVersionConstraint = Nothing, depType = BuildDepends }
      
      -- Determine which sections to remove from
      let targets = case roSection opts of
            TargetLib -> 
              -- Smart detection: find all sections that HAVE this dependency
              let allSections = cfSections cabalFile
                  matchingSections = filter (hasDependency pkgName) allSections
              in if null matchingSections 
                 then [TargetLib] -- Default to library if none found (will fail later with "not found")
                 else map sectionToTarget matchingSections
            other -> [other]
            
      -- Apply removal to all targets
      let result = foldl (applyRemoval pkgName dep eol leadingComma cabalFile) (Success currentContent) targets
      
      -- If nothing was removed (content same as before), and we didn't already have a failure, return error
      case result of
        Success newContent | newContent == currentContent ->
          return $ Failure $ Error ("Dependency not found: " <> unPackageName pkgName) InvalidDependency
        other -> return other

hasDependency :: PackageName -> Section -> Bool
hasDependency name sec = any (\d -> depName d == name) (findDependencies sec)

sectionToTarget :: Section -> SectionTarget
sectionToTarget (LibrarySection lib) = case libName lib of
  Nothing -> TargetLib
  Just n -> TargetNamed n
sectionToTarget (ExecutableSection exe) = TargetExe (Just $ exeName exe)
sectionToTarget (TestSuiteSection test) = TargetTest (Just $ testName test)
sectionToTarget (BenchmarkSection bench) = TargetBench (Just $ benchName bench)
sectionToTarget (CommonStanzaSection common) = TargetNamed (commonName common)
sectionToTarget (FlagSection f) = TargetCommon (Just $ flagName f) -- Reusing TargetCommon or add TargetFlag?
-- Let's use TargetNamed for now as it's safe.
-- Actually, let's use TargetNamed.
sectionToTarget (UnknownSection name _) = TargetNamed name

applyRemoval :: PackageName -> Dependency -> Text -> Bool -> CabalFile -> Result Text -> SectionTarget -> Result Text
applyRemoval _ _ _ _ _ (Failure err) _ = Failure err
applyRemoval pkgName dep eol leadingComma cabalFile (Success content) target =
  case resolveTargetBounds target cabalFile content of
    Left err -> Failure err
    Right (start, end, _, _) ->
       let (before, rest) = T.splitAt start content
           (sectionContent, after) = T.splitAt (end - start) rest
       in if not (unPackageName pkgName `T.isInfixOf` sectionContent)
          then Success content -- Skip if not in this section
          else Success $ before <> removeDependencyLine eol leadingComma dep sectionContent <> after

-- ... keep helper describeSection if needed or reuse ...

