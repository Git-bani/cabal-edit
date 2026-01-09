{-# LANGUAGE OverloadedStrings #-}
module Business.Sync (syncWorkspace) where

import Core.Types
import Core.AST.Types (CabalAST)
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (findDependenciesInAST, updateDependencyInAST)
import Core.ProjectContext (ProjectContext(..))
import Core.DependencyResolver (resolveVersionConstraint)
import Core.Safety (safeWriteFile)
import Utils.Logging (logInfo, logError, logSuccess)
import Utils.Diff (diffLines, colorizeDiff)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM, forM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (mapMaybe, listToMaybe)

-- | Sync dependency versions across the workspace
syncWorkspace :: SyncOptions -> ProjectContext -> IO (Either Error ())
syncWorkspace opts ctx = do
  let packages = pcPackages ctx
  logInfo $ "Found " <> T.pack (show (length packages)) <> " packages in workspace."
  
  -- 1. Load all ASTs and find all dependencies
  packageData <- mapM loadPackageData packages
  
  -- 2. Build a map of "latest used" or "hackage latest" version for each package
  masterMapResult <- if soLatest opts
                     then buildHackageLatestMap packageData
                     else return $ Right $ buildLocalLatestMap packageData
  
  case masterMapResult of
    Left err -> return $ Left err
    Right masterMap -> do
      -- 3. Apply updates to each package
      results <- forM packageData $ \(name, path, ast, content) -> do
        let deps = findDependenciesInAST ast
        let updates = mapMaybe (findUpdate masterMap) deps
        
        if null updates
          then do
            logInfo $ "Package " <> unPackageName name <> " is already synced."
            return $ Right ()
          else do
            let finalASTResult = foldM applyUpdate ast updates
            case finalASTResult of
              Left err -> do
                logError $ "Failed to update " <> unPackageName name <> ": " <> errorMessage err
                return $ Left err
              Right finalAST -> do
                let finalContent = serializeAST finalAST
                if finalContent == content
                  then return $ Right ()
                  else if soDryRun opts
                    then do
                      logInfo $ "Dry run: Syncing " <> unPackageName name <> " (" <> T.pack path <> "):"
                      let diffs = diffLines (T.lines content) (T.lines finalContent)
                      colorizeDiff diffs
                      return $ Right ()
                    else do
                      logInfo $ "Syncing " <> unPackageName name <> "..."
                      safeWriteFile path finalContent
      
      case listToMaybe [e | Left e <- results] of
        Nothing -> do
          logSuccess "Workspace sync complete."
          return $ Right ()
        Just err -> return $ Left err

type PackageData = (PackageName, FilePath, CabalAST, Text)

loadPackageData :: (PackageName, FilePath) -> IO PackageData
loadPackageData (name, path) = do
  content <- TIO.readFile path
  let ast = parseAST content
  return (name, path, ast, content)

-- | Build a map of the highest version found in the workspace for each package
buildLocalLatestMap :: [PackageData] -> Map PackageName VersionConstraint
buildLocalLatestMap pkgData = 
  let allDeps = concatMap (\(_, _, ast, _) -> findDependenciesInAST ast) pkgData
      -- Map PackageName -> [VersionConstraint]
      grouped = foldl' insertDep Map.empty allDeps
  in Map.map pickBestConstraint grouped
  where
    insertDep m (_, _, dep) = 
      let name = depName dep
          constraint = case depVersionConstraint dep of 
                         Just c -> c
                         Nothing -> AnyVersion
      in Map.insertWith (++) name [constraint] m

-- | Pick the "best" (usually highest) version constraint from a list
-- This is a heuristic. For now, we prefer MajorBound (caret) or specific versions.
pickBestConstraint :: [VersionConstraint] -> VersionConstraint
pickBestConstraint [] = AnyVersion
pickBestConstraint cs = maximum cs -- VersionConstraint implements Ord

-- | Build a map of the latest version from Hackage for all shared dependencies
buildHackageLatestMap :: [PackageData] -> IO (Either Error (Map PackageName VersionConstraint))
buildHackageLatestMap pkgData = do
  let allDeps = concatMap (\(_, _, ast, _) -> findDependenciesInAST ast) pkgData
  let uniqueNames = nub $ map (\(_, _, d) -> depName d) allDeps
  logInfo $ "Fetching latest versions for " <> T.pack (show (length uniqueNames)) <> " unique dependencies..."
  
  -- We don't have a project context for individual packages here, but we can pass Nothing
  -- or use the workspace context if we had it.
  results <- forM uniqueNames $ \name -> do
    res <- resolveVersionConstraint Nothing Nothing name Nothing
    return (name, res)
  
  let (failures, successes) = partitionResults results
  case listToMaybe failures of
    Just err -> return $ Left err
    Nothing -> return $ Right $ Map.fromList successes

partitionResults :: [(a, Either b c)] -> ([b], [(a, c)])
partitionResults [] = ([], [])
partitionResults ((a, res):xs) = 
  let (fs, ss) = partitionResults xs
  in case res of 
       Left e -> (e:fs, ss)
       Right v -> (fs, (a, v):ss)

findUpdate :: Map PackageName VersionConstraint -> (Text, Maybe Text, Dependency) -> Maybe (Text, Maybe Text, Dependency)
findUpdate masterMap (sec, mCond, dep) = 
  case Map.lookup (depName dep) masterMap of 
    Just masterConstraint -> 
      if Just masterConstraint /= depVersionConstraint dep
      then Just (sec, mCond, dep { depVersionConstraint = Just masterConstraint })
      else Nothing
    Nothing -> Nothing

applyUpdate :: CabalAST -> (Text, Maybe Text, Dependency) -> Either Error CabalAST
applyUpdate ast (sec, mCond, dep) = updateDependencyInAST sec mCond dep ast
