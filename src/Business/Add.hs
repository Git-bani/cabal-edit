{-# LANGUAGE OverloadedStrings #-}

module Business.Add (addDependency) where

import Core.Types
import Core.Parser
import Core.Serializer
import Core.Safety
import Core.DependencyResolver
import Core.ProjectEditor
import Core.ProjectContext (ProjectContext)
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM)

addDependency :: Maybe ProjectContext -> AddOptions -> FilePath -> IO (Result ())
addDependency maybeCtx opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  
  -- 0. Handle Source Dependencies (Git / Path)
  sourceDepResult <- handleSourceDependency opts
  case sourceDepResult of
    Failure err -> return $ Failure err
    Success () -> do
      -- 1. Parse cabal file
      parseResult <- parseCabalFile path
      case parseResult of
        Failure err -> return $ Failure err
        Success cabalFile -> do
          -- 2. Process each package
          let eol = cfLineEndings cabalFile
          let initialContent = cfRawContent cabalFile
          
          finalContentResult <- foldM (processPackage maybeCtx opts eol leadingComma cabalFile) (Success initialContent) (aoPackageNames opts)
          
          case finalContentResult of
            Failure err -> return $ Failure err
            Success finalContent -> 
              if aoDryRun opts
                then do
                  logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                  TIO.putStrLn finalContent
                  return $ Success ()
                else safeWriteCabal path finalContent

processPackage :: Maybe ProjectContext -> AddOptions -> Text -> Bool -> CabalFile -> Result Text -> Text -> IO (Result Text)
processPackage _ _ _ _ _ (Failure err) _ = return $ Failure err
processPackage maybeCtx opts eol leadingComma cabalFile (Success currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Failure $ Error err InvalidDependency
    Right pkgName -> do
      -- Resolve version / Constraint
      let isSourceDep = isJust (aoGit opts) || isJust (aoPath opts)
      constraintResult <- if isSourceDep && isNothing (aoVersion opts)
                          then return $ Success AnyVersion
                          else resolveVersionConstraint maybeCtx pkgName (aoVersion opts)
      
      case constraintResult of
        Failure err -> return $ Failure err
        Success constraint -> do
          -- Determine target section (re-finding in the list of sections from parsed file)
          -- Note: sections positions might shift if we were editing the whole file at once, 
          -- but here we are accumulating changes in `currentContent`.
          -- We need to find the section again in the updated content.
          -- Actually, findSection returns a Section with position.
          -- If we edit one section, other sections' positions might change.
          -- Strategy: Always use `cabalFile` to find section names, but use a fresh search in `currentContent`.
          
          case findSection (aoSection opts) cabalFile of
            Nothing -> return $ Failure $ Error "Section not found" FileNotFound
            Just sec -> do
              -- Create dependency
              let dep = Dependency
                    { depName = pkgName
                    , depVersionConstraint = Just constraint
                    , depType = if aoDev opts then TestDepends else BuildDepends
                    }
              
              -- We need fresh bounds for the section in the current (possibly modified) content
              let secHeader = describeSection sec
              let TextSpan (TextOffset start) (TextOffset end) = findSectionPosition secHeader currentContent
              
              let (before, rest) = T.splitAt start currentContent
              let (sectionContent, after) = T.splitAt (end - start) rest
              
              -- Check if dependency exists in THIS section
              -- (Ideally we'd parse sectionContent to get deps, but for now we can check raw text or rely on findDependencies from original sec)
              let alreadyExists = unPackageName pkgName `T.isInfixOf` sectionContent -- simple check
              
              let newSectionContent = if alreadyExists
                    then updateDependencyLine eol leadingComma dep sectionContent
                    else insertDependencyLine eol leadingComma dep sectionContent
                    
              return $ Success $ before <> newSectionContent <> after

describeSection :: Section -> Text
describeSection (LibrarySection lib) = case libName lib of
  Nothing -> "library"
  Just n -> "library " <> n
describeSection (ExecutableSection exe) = "executable " <> exeName exe
describeSection (TestSuiteSection test) = "test-suite " <> testName test
describeSection (BenchmarkSection bench) = "benchmark " <> benchName bench
describeSection (CommonStanzaSection common) = "common " <> commonName common
describeSection (UnknownSection name _) = name

handleSourceDependency :: AddOptions -> IO (Result ())
handleSourceDependency opts
  | aoDryRun opts = return $ Success () -- Skip project mod in dry-run for now
  | Just gitUrl <- aoGit opts = do
      (projPath, _) <- ensureProjectFile
      logInfo $ "Adding git dependency to " <> T.pack projPath
      addSourceRepository projPath gitUrl (aoTag opts)
  | Just localPath <- aoPath opts = do
      (projPath, _) <- ensureProjectFile
      logInfo $ "Adding local dependency to " <> T.pack projPath
      addLocalPackage projPath localPath
  | otherwise = return $ Success ()

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False
