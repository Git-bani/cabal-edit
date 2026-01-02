{-# LANGUAGE OverloadedStrings #-}

module Business.Add (addDependency) where

import Core.Types
import Core.Parser
import Core.Serializer (insertDependencyLine, updateDependencyLine, detectIndentation)
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
import Control.Monad (foldM, forM_)

import Utils.Diff (diffLines, colorizeDiff)

addDependency :: Maybe ProjectContext -> AddOptions -> FilePath -> IO (Result ())
addDependency maybeCtx opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  
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
              let eol = cfLineEndings cabalFile
              let initialContent = cfRawContent cabalFile
              
              finalContentResult <- foldM (processPackage maybeCtx opts eol leadingComma cabalFile) (Success initialContent) targetPkgNames
              
              case finalContentResult of
                Failure err -> return $ Failure err
                Success finalContent -> 
                  if aoDryRun opts
                    then do
                      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                      let diffs = diffLines (T.lines initialContent) (T.lines finalContent)
                      colorizeDiff diffs
                      return $ Success ()
                    else safeWriteCabal path finalContent

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
          
          -- Determine target section/block
          let baseTarget = aoSection opts
          let condition = case aoCondition opts of
                            Just c -> Just c
                            Nothing -> case aoFlag opts of
                                         Just f -> Just ("flag(" <> f <> ")")
                                         Nothing -> Nothing

          let target = case condition of
                         Nothing -> baseTarget
                         Just cond -> TargetConditional baseTarget cond
          
          case resolveTargetBounds target cabalFile currentContent of
            Left err -> return $ Failure err
            Right (start, end, prefix, suffix) -> do
              -- Create dependency
              let dep = Dependency
                    { depName = pkgName
                    , depVersionConstraint = Just constraint
                    , depType = if aoDev opts then TestDepends else BuildDepends
                    }
              
              let (before, rest) = T.splitAt start currentContent
              let actualSectionContent = if T.null prefix 
                                         then T.take (end - start) rest
                                         else 
                                           -- Creating new block.
                                           -- We need to know the base indent.
                                           let bIndent = detectIndentation currentContent
                                               indentStr = T.replicate (bIndent + 4) " "
                                           in indentStr <> "build-depends: "
              
              let after = if T.null prefix then T.drop (end - start) rest else rest
              
              -- Check if dependency exists in THIS section
              let alreadyExists = if T.null prefix 
                                  then unPackageName pkgName `T.isInfixOf` actualSectionContent
                                  else False
              
              let newSectionContent = if alreadyExists
                    then updateDependencyLine eol leadingComma dep actualSectionContent
                    else insertDependencyLine eol leadingComma dep actualSectionContent
                    
              return $ Success $ before <> prefix <> newSectionContent <> suffix <> after

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
