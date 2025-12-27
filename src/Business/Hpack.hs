{-# LANGUAGE OverloadedStrings #-}

module Business.Hpack (addHpackDependency, removeHpackDependency) where

import Core.Types
import Core.HpackEditor
import Core.Serializer (DependencyOperation(..))
import Core.Safety
import Core.DependencyResolver
import Utils.Logging (logInfo)
import Utils.Terminal (selectItems)
import Utils.Diff (diffLines, colorizeDiff)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM)
import Data.List (nub)
import Data.Maybe (mapMaybe)

addHpackDependency :: AddOptions -> FilePath -> IO (Result ())
addHpackDependency opts path = do
  content <- TIO.readFile path
  
  finalContentResult <- foldM (processHpackPackage opts) (Success content) (aoPackageNames opts)
  
  case finalContentResult of
    Failure err -> return $ Failure err
    Success finalContent -> 
      if aoDryRun opts
        then do
          logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
          let diffs = diffLines (T.lines content) (T.lines finalContent)
          colorizeDiff diffs
          return $ Success ()
        else do
          -- Using safeWriteFile instead of safeWriteCabal as it doesn't need Cabal verification
          _ <- safeWriteFile path finalContent
          return $ Success ()

processHpackPackage :: AddOptions -> Result Text -> Text -> IO (Result Text)
processHpackPackage _ (Failure err) _ = return $ Failure err
processHpackPackage opts (Success currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Failure $ Error err InvalidDependency
    Right pkgName -> do
      -- Resolve version / Constraint
      -- For Hpack we don't need full ProjectContext for local resolution yet (simplified)
      constraintResult <- resolveVersionConstraint Nothing pkgName (aoVersion opts)
      
      case constraintResult of
        Failure err -> return $ Failure err
        Success constraint -> do
          let dep = Dependency
                { depName = pkgName
                
                , depVersionConstraint = Just constraint
                , depType = if aoDev opts then TestDepends else BuildDepends
                }
          
          -- Hpack editing is currently global (adding to top-level dependencies)
          -- TODO: Support sections in Hpack
          return $ Success $ updateHpackDependencies currentContent [dep] Add

removeHpackDependency :: RemoveOptions -> FilePath -> IO (Result ())
removeHpackDependency opts path = do
  content <- TIO.readFile path
  
  -- Handle interactive mode
  packageNames <- if roInteractive opts
    then do
      -- Crude extraction of dependencies from package.yaml
      let ls = T.lines content
      let deps = extractHpackDeps ls
      if null deps
        then return []
        else selectItems "Select dependencies to remove (Hpack):" (nub deps)
    else return (roPackageNames opts)

  let finalContent = foldr (\pkgNameText acc -> 
        case mkPackageName pkgNameText of
          Left _ -> acc
          Right pkgName -> 
            let dep = Dependency { depName = pkgName, depVersionConstraint = Nothing, depType = BuildDepends }
            in updateHpackDependencies acc [dep] Remove
        ) content packageNames
  
  if roDryRun opts
    then do
      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
      let diffs = diffLines (T.lines content) (T.lines finalContent)
      colorizeDiff diffs
      return $ Success ()
    else do
      _ <- safeWriteFile path finalContent
      return $ Success ()

extractHpackDeps :: [Text] -> [Text]
extractHpackDeps ls = 
  let isDepLine l = "-" `T.isPrefixOf` T.strip l
      getPkg l = T.words (T.drop 1 (T.strip l))
  in mapMaybe (\l -> case getPkg l of (p:_) -> Just p; _ -> Nothing) (filter isDepLine ls)