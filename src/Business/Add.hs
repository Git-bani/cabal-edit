{-# LANGUAGE OverloadedStrings #-}

module Business.Add (addDependency) where

import Core.Types
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (addDependencyToAST, getCabalVersion, addMixinToAST)
import Core.Safety
import Core.DependencyResolver
import Core.ProjectEditor
import Core.ProjectContext (ProjectContext(..))
import Core.Solver (verifyChanges)
import Utils.Logging (logInfo, logError, logWarning)
import Utils.Terminal (selectPackages)
import External.Hackage (searchPackages)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, mapMaybe)

import Utils.Formatting (describeTarget)
import Utils.Diff (diffLines, colorizeDiff)

addDependency :: Maybe ProjectContext -> AddOptions -> FilePath -> IO (Either Error ())
addDependency maybeCtx opts path = do
  -- Check for freeze file
  case maybeCtx of
    Just ctx | pcHasFreezeFile ctx -> 
      logWarning "cabal.project.freeze detected. This might override your dependency changes!"
    _ -> return ()

  -- 0. Handle Interactive Search
  pkgNamesResult <- if aoInteractive opts
                    then handleInteractiveSearch (aoPackageNames opts)
                    else return $ Right (aoPackageNames opts)

  case pkgNamesResult of
    Left err -> return $ Left err
    Right targetPkgNames -> do
      -- 0. Handle Source Dependencies (Git / Path)
      sourceDepResult <- handleSourceDependency opts
      case sourceDepResult of
        Left err -> return $ Left err
        Right () -> do
          -- 1. Read file
          content <- TIO.readFile path
          
          -- 2. Process each package
          finalContentResult <- foldM (processPackage maybeCtx opts) (Right content) targetPkgNames
          
          case finalContentResult of
            Left err -> return $ Left err
            Right finalContent -> do
              -- Solver Check
              verificationResult <- if aoCheck opts
                                    then verifyChanges path finalContent
                                    else return $ Right ()
              
              case verificationResult of
                Left err -> return $ Left err
                Right () -> 
                  if aoDryRun opts
                    then do
                      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                      let diffs = diffLines (T.lines content) (T.lines finalContent)
                      colorizeDiff diffs
                      return $ Right ()
                    else safeWriteFile path finalContent

handleInteractiveSearch :: [Text] -> IO (Either Error [Text])
handleInteractiveSearch [] = do
  logError "Please provide a search term for interactive mode: 'add -i <term>'"
  return $ Right []
handleInteractiveSearch terms = do
  let query = T.intercalate " " terms
  logInfo $ "Searching Hackage for '" <> query <> "'..."
  searchResult <- searchPackages query
  case searchResult of
    Left err -> return $ Left err
    Right [] -> do
      logWarning "No packages found matching your search."
      return $ Right []
    Right results -> do
      selected <- selectPackages "Select packages to add:" results
      return $ Right selected

processPackage :: Maybe ProjectContext -> AddOptions -> Either Error Text -> Text -> IO (Either Error Text)
processPackage _ _ (Left err) _ = return $ Left err
processPackage maybeCtx opts (Right currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Left $ Error err InvalidDependency
    Right pkgName -> do
      -- Parse AST
      let ast = parseAST currentContent
      let cabalVer = getCabalVersion ast
      
      -- Resolve version / Constraint
      let isSourceDep = isJust (aoGit opts) || isJust (aoPath opts)
      constraintResult <- if isSourceDep && isNothing (aoVersion opts)
                          then return $ Right AnyVersion
                          else resolveVersionConstraint maybeCtx cabalVer pkgName (aoVersion opts) (aoStrategy opts)
      
      case constraintResult of
        Left err -> return $ Left err
        Right constraint -> do
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
          let targetName = case baseTarget of
                             TargetNamed n -> n
                             _ -> describeTarget baseTarget
          
          let depResult = addDependencyToAST targetName condition dep ast
          
          case depResult of
            Left err -> return $ Left err
            Right astWithDep -> 
              -- Handle Mixin if present
              case aoMixin opts of
                Nothing -> return $ Right $ serializeAST astWithDep
                Just mixinStr -> do
                  let renaming = parseRenaming mixinStr
                  let mixin = Mixin { mixinPackage = pkgName, mixinRenaming = renaming }
                  case addMixinToAST targetName condition mixin astWithDep of
                    Left err -> return $ Left err
                    Right finalAST -> return $ Right $ serializeAST finalAST

parseRenaming :: Text -> Renaming
parseRenaming t =
  let trimmed = T.strip t
  in if "hiding" `T.isPrefixOf` T.toLower trimmed
     then 
       let content = T.drop 6 trimmed -- drop "hiding"
           -- clean = T.dropWhile (not . isAlphaNum') content -- drop parens/spaces
           -- This is a simplistic parser, could be improved
           mods = map T.strip $ T.splitOn "," (T.filter (`notElem` ['(',')']) content)
       in Hiding (filter (not . T.null) mods)
     else 
       let content = T.filter (`notElem` ['(',')']) trimmed
           parts = map T.strip $ T.splitOn "," content
           parseRename p = 
             let ws = T.words p
             in case ws of
                  [old, "as", new] -> Just (old, new)
                  _ -> Nothing
           renames = mapMaybe parseRename parts
       in if null renames then DefaultRenaming else Renaming renames

handleSourceDependency :: AddOptions -> IO (Either Error ())
handleSourceDependency opts
  | aoDryRun opts = return $ Right () -- Skip project mod in dry-run for now
  | Just gitUrl <- aoGit opts = do
      (projPath, _) <- ensureProjectFile
      logInfo $ "Adding git dependency to " <> T.pack projPath
      addSourceRepository projPath gitUrl (aoTag opts)
  | Just localPath <- aoPath opts = do
      (projPath, _) <- ensureProjectFile
      logInfo $ "Adding local dependency to " <> T.pack projPath
      addLocalPackage projPath localPath
  | otherwise = return $ Right ()

