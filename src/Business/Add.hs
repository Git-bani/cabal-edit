{-# LANGUAGE OverloadedStrings #-}

module Business.Add (addDependency) where

import Core.Types
import Core.Parser
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (addDependencyToAST)
import Core.Safety
import Core.DependencyResolver
import Core.ProjectEditor
import Core.ProjectContext (ProjectContext)
import Utils.Logging (logInfo, logError, logWarning)
import Utils.Config (loadConfig, Config(..))
import Utils.Terminal (selectItems)
import External.Hackage (searchPackages)
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Control.Monad (foldM)
import Data.Maybe (maybeToList)

import Utils.Diff (diffLines, colorizeDiff)

addDependency :: Maybe ProjectContext -> AddOptions -> FilePath -> IO (Result ())
addDependency maybeCtx opts path = do
  -- 0. Handle Interactive Search
  pkgNamesResult <- if aoInteractive opts
                    then handleInteractiveSearch (aoPackageNames opts)
                    else return $ Success (aoPackageNames opts)

  case pkgNamesResult of
    Failure err -> return $ Failure err
    Success targetPkgNames -> do
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
              let initialContent = cfRawContent cabalFile
              
              finalContentResult <- foldM (processPackage maybeCtx opts) (Success initialContent) targetPkgNames
              
              case finalContentResult of
                Failure err -> return $ Failure err
                Success finalContent -> 
                  if aoDryRun opts
                    then do
                      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                      let diffs = diffLines (T.lines initialContent) (T.lines finalContent)
                      colorizeDiff diffs
                      return $ Success ()
                    else safeWriteFile path finalContent

handleInteractiveSearch :: [Text] -> IO (Result [Text])
handleInteractiveSearch [] = do
  logError "Please provide a search term for interactive mode: 'add -i <term>'"
  return $ Success []
handleInteractiveSearch terms = do
  let query = T.intercalate " " terms
  logInfo $ "Searching Hackage for '" <> query <> "'..."
  searchResult <- searchPackages query
  case searchResult of
    Failure err -> return $ Failure err
    Success [] -> do
      logWarning "No packages found matching your search."
      return $ Success []
    Success results -> do
      selected <- selectItems "Select packages to add:" results
      return $ Success selected

processPackage :: Maybe ProjectContext -> AddOptions -> Result Text -> Text -> IO (Result Text)
processPackage _ _ (Failure err) _ = return $ Failure err
processPackage maybeCtx opts (Success currentContent) pkgNameText = do
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
          let dep = Dependency
                { depName = pkgName
                , depVersionConstraint = Just constraint
                , depType = if aoDev opts then TestDepends else BuildDepends
                }
          
          -- Determine target section/block
          let baseTarget = aoSection opts
          let condition = case aoCondition opts of
                            Just c -> Just c
                            Nothing -> case aoFlag opts of
                                         Just f -> Just ("flag(" <> f <> ")")
                                         Nothing -> Nothing

          -- Use AST Editor for all additions
          let ast = parseAST currentContent
          let targetName = case baseTarget of
                             TargetNamed n -> n
                             _ -> describeTarget baseTarget
          case addDependencyToAST targetName condition dep ast of
            Failure err -> return $ Failure err
            Success updatedAST -> return $ Success $ serializeAST updatedAST



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

describeTarget :: SectionTarget -> Text
describeTarget TargetLib = "library"
describeTarget (TargetNamed n) = n 
describeTarget (TargetExe mn) = T.unwords $ ["executable"] ++ maybeToList mn
describeTarget (TargetTest mn) = T.unwords $ ["test-suite"] ++ maybeToList mn
describeTarget (TargetBench mn) = T.unwords $ ["benchmark"] ++ maybeToList mn
describeTarget (TargetCommon mn) = T.unwords $ ["common"] ++ maybeToList mn
describeTarget (TargetConditional base _) = describeTarget base

