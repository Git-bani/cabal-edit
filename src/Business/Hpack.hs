{-# LANGUAGE OverloadedStrings #-}

module Business.Hpack (addHpackDependency, removeHpackDependency) where

import Core.Types
import Core.HpackEditor
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

addHpackDependency :: AddOptions -> FilePath -> IO (Either Error ())
addHpackDependency opts path = do
  content <- TIO.readFile path
  
  finalContentResult <- foldM (processHpackPackage opts) (Right content) (aoPackageNames opts)
  
  case finalContentResult of
    Left err -> return $ Left err
    Right finalContent -> 
      if aoDryRun opts
        then do
          logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
          let diffs = diffLines (T.lines content) (T.lines finalContent)
          colorizeDiff diffs
          return $ Right ()
        else do
          -- Using safeWriteFile instead of safeWriteCabal as it doesn't need Cabal verification
          _ <- safeWriteFile path finalContent
          return $ Right ()

processHpackPackage :: AddOptions -> Either Error Text -> Text -> IO (Either Error Text)
processHpackPackage _ (Left err) _ = return $ Left err
processHpackPackage opts (Right currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Left $ Error err InvalidDependency
    Right pkgName -> do
      -- Resolve version / Constraint
      -- For Hpack we don't need full ProjectContext for local resolution yet (simplified)
      constraintResult <- resolveVersionConstraint Nothing Nothing pkgName (aoVersion opts)
      
      case constraintResult of
        Left err -> return $ Left err
        Right constraint -> do
          let dep = Dependency
                { depName = pkgName
                
                , depVersionConstraint = Just constraint
                , depType = if aoDev opts then TestDepends else BuildDepends
                }
          
          -- Hpack editing is currently global (adding to top-level dependencies)
          -- TODO: Support sections in Hpack
          return $ Right $ updateHpackDependencies currentContent [dep] Add

removeHpackDependency :: RemoveOptions -> FilePath -> IO (Either Error ())
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
      return $ Right ()
    else do
      _ <- safeWriteFile path finalContent
      return $ Right ()

extractHpackDeps :: [Text] -> [Text]
extractHpackDeps ls = 
  let isDepLine l = "-" `T.isPrefixOf` T.strip l
      getPkg l = T.words (T.drop 1 (T.strip l))
  in mapMaybe (\l -> case getPkg l of (p:_) -> Just p; _ -> Nothing) (filter isDepLine ls)