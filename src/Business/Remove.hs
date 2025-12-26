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
              TIO.putStrLn finalContent
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
      let result = foldl (applyRemoval pkgName dep eol leadingComma) (Success currentContent) targets
      
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
sectionToTarget (UnknownSection name _) = TargetNamed name

applyRemoval :: PackageName -> Dependency -> Text -> Bool -> Result Text -> SectionTarget -> Result Text
applyRemoval _ _ _ _ (Failure err) _ = Failure err
applyRemoval pkgName dep eol leadingComma (Success content) target =
  -- We need to find the section position in the CURRENT content
  -- Use a helper to find section start by target name
  let secHeader = targetToHeader target
      TextSpan (TextOffset start) (TextOffset end) = findSectionPosition secHeader content
  in if start == 0 && end == 0
     then Failure $ Error ("Section not found: " <> secHeader) FileNotFound
     else
       let (before, rest) = T.splitAt start content
           (sectionContent, after) = T.splitAt (end - start) rest
       in if not (unPackageName pkgName `T.isInfixOf` sectionContent)
          then Success content -- Skip if not in this section
          else Success $ before <> removeDependencyLine eol leadingComma dep sectionContent <> after

targetToHeader :: SectionTarget -> Text
targetToHeader TargetLib = "library"
targetToHeader (TargetExe Nothing) = "executable"
targetToHeader (TargetExe (Just n)) = "executable " <> n
targetToHeader (TargetTest Nothing) = "test-suite"
targetToHeader (TargetTest (Just n)) = "test-suite " <> n
targetToHeader (TargetBench Nothing) = "benchmark"
targetToHeader (TargetBench (Just n)) = "benchmark " <> n
targetToHeader (TargetCommon Nothing) = "common"
targetToHeader (TargetCommon (Just n)) = "common " <> n
targetToHeader (TargetNamed n) = n

-- ... keep helper describeSection if needed or reuse ...

