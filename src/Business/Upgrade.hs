{-# LANGUAGE OverloadedStrings #-}
module Business.Upgrade
 (upgradeDependencies) where

import Core.Types
import Core.Parser
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (updateDependencyInAST, findDependenciesInAST)
import Core.Safety
import Core.DependencyResolver (resolveLatestVersion)
import Utils.Logging (logInfo)
import Utils.Terminal (selectItems)
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Control.Monad (forM, foldM)
import Data.List (nub, find)

import Utils.Diff (diffLines, colorizeDiff)

upgradeDependencies :: UpgradeOptions -> FilePath -> IO (Result ())
upgradeDependencies opts path = do
  -- 1. Parse into AST
  parseResult <- parseCabalFile path
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      let content = cfRawContent cabalFile
      let ast = parseAST content
      
      -- 2. Gathering all upgradeable dependencies
      let allDepsWithLoc = findDependenciesInAST ast
  
      -- 3. Filter by user requested packages
      targetPkgNamesResult <- if null (uoPackageNames opts)
                              then return $ Right []
                              else let results = map (\n -> (n, mkPackageName n)) (uoPackageNames opts)
                                       errors = [n | (n, Left _) <- results]
                                       valid = [p | (_, Right p) <- results]
                                   in if null errors 
                                      then return $ Right valid
                                      else return $ Left ("Invalid package names: " <> T.intercalate ", " errors)

      case targetPkgNamesResult of
        Left err -> return $ Failure $ Error err InvalidDependency
        Right userPkgNames -> do
          let depsToUpgrade = if null userPkgNames
                              then allDepsWithLoc
                              else filter (\(_, _, d) -> depName d `elem` userPkgNames) allDepsWithLoc
          
          if null depsToUpgrade
            then return $ Success ()
            else do
              -- 4. Get unique package names to upgrade
              let uniquePkgNames = nub $ map (depName . thd) depsToUpgrade
              
              -- 5. Resolve latest versions for these packages
              upgradedDepsResult <- forM uniquePkgNames resolveLatestDependency
              let failures = [e | Failure e <- upgradedDepsResult]
              case failures of
                (e:_) -> return $ Failure e
                [] -> do
                  let upgradedDeps = [d | Success d <- upgradedDepsResult]
                  
                  -- 6. Filter by interactive selection
                  potentialUpgrades <- if uoInteractive opts
                    then do
                      let items = map formatUpgrade upgradedDeps
                      selectedItems <- selectItems "Select packages to upgrade:" items
                      return $ filter (\u -> formatUpgrade u `elem` selectedItems) upgradedDeps
                    else return upgradedDeps
                  
                  if null potentialUpgrades
                    then return $ Success ()
                    else do
                      -- 7. Apply upgrades
                      let finalASTResult = foldM applyUpgrade ast depsToUpgrade
                            where
                              applyUpgrade currentAst (sec, mCond, oldDep) =
                                case find (\u -> depName u == depName oldDep) potentialUpgrades of
                                  Just newDep -> updateDependencyInAST sec mCond newDep currentAst
                                  Nothing -> Success currentAst

                      case finalASTResult of
                        Failure err -> return $ Failure err
                        Success finalAST -> do
                          let newContent = serializeAST finalAST
                          if newContent /= content
                            then 
                              if uoDryRun opts
                                then do
                                  logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                                  let diffs = diffLines (T.lines content) (T.lines newContent)
                                  colorizeDiff diffs
                                  return $ Success ()
                                else safeWriteFile path newContent
                            else return $ Success ()

thd :: (a, b, c) -> c
thd (_, _, c) = c

resolveLatestDependency :: PackageName -> IO (Result Dependency)
resolveLatestDependency name = do
  res <- resolveLatestVersion name
  case res of
    Failure err -> return $ Failure err
    Success ver -> return $ Success $ Dependency 
      { depName = name
      , depVersionConstraint = Just (MajorBoundVersion ver)
      , depType = BuildDepends 
      }

formatUpgrade :: Dependency -> Text
formatUpgrade dep = 
  let verStr = case depVersionConstraint dep of
                 Just (MajorBoundVersion (Version parts)) -> "^>=" <> T.intercalate "." (map (T.pack . show) parts)
                 _ -> ""
  in unPackageName (depName dep) <> " " <> verStr
