{-# LANGUAGE OverloadedStrings #-}
module Business.Upgrade
 (upgradeDependencies) where

import Core.Types
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST, formatDependency)
import Core.AST.Editor (updateDependencyInAST, findDependenciesInAST, getCabalVersion)
import Core.Safety
import Core.DependencyResolver (resolveVersionConstraint)
import Utils.Logging (logInfo)
import Utils.Terminal (selectItems)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM, foldM)
import Data.List (nub, find)

import Utils.Diff (diffLines, colorizeDiff)

upgradeDependencies :: UpgradeOptions -> FilePath -> IO (Either Error ())
upgradeDependencies opts path = do
  -- 1. Read file and parse AST
  content <- TIO.readFile path
  let ast = parseAST content
  let cabalVer = getCabalVersion ast
  
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
                                  else return $ Left (Error ("Invalid package names: " <> T.intercalate ", " errors) InvalidDependency)

  case targetPkgNamesResult of
    Left err -> return $ Left err
    Right userPkgNames -> do
      let depsToUpgrade = if null userPkgNames
                          then allDepsWithLoc
                          else filter (\(_, _, d) -> depName d `elem` userPkgNames) allDepsWithLoc
      
      if null depsToUpgrade
        then return $ Right ()
        else do
          -- 4. Get unique package names to upgrade
          let uniquePkgNames = nub $ map (depName . thd) depsToUpgrade
          
          -- 5. Resolve latest versions for these packages
          upgradedDepsResult <- forM uniquePkgNames (resolveLatestDependency cabalVer)
          let failures = [e | Left e <- upgradedDepsResult]
          case failures of
            (e:_) -> return $ Left e
            [] -> do
              let upgradedDeps = [d | Right d <- upgradedDepsResult]
              
              -- 6. Filter by interactive selection
              potentialUpgrades <- if uoInteractive opts
                then do
                  let items = map formatUpgrade upgradedDeps
                  selectedItems <- selectItems "Select packages to upgrade:" items
                  return $ filter (\u -> formatUpgrade u `elem` selectedItems) upgradedDeps
                else return upgradedDeps
              
              if null potentialUpgrades
                then return $ Right ()
                else do
                  -- 7. Apply upgrades
                  let finalASTResult = foldM applyUpgrade ast depsToUpgrade
                        where
                          applyUpgrade currentAst (sec, mCond, oldDep) =
                            case find (\u -> depName u == depName oldDep) potentialUpgrades of
                              Just newDep -> updateDependencyInAST sec mCond newDep currentAst
                              Nothing -> Right currentAst

                  case finalASTResult of
                    Left err -> return $ Left err
                    Right finalAST -> do
                      let newContent = serializeAST finalAST
                      if newContent /= content
                        then 
                          if uoDryRun opts
                            then do
                              logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                              let diffs = diffLines (T.lines content) (T.lines newContent)
                              colorizeDiff diffs
                              return $ Right ()
                            else safeWriteFile path newContent
                        else return $ Right ()

thd :: (a, b, c) -> c
thd (_, _, c) = c

resolveLatestDependency :: Maybe Version -> PackageName -> IO (Either Error Dependency)
resolveLatestDependency cabalVer name = do
  -- Resolve using logic (PVP, Hackage)
  res <- resolveVersionConstraint Nothing cabalVer name Nothing
  case res of
    Left err -> return $ Left err
    Right constraint -> return $ Right $ Dependency 
      { depName = name
      , depVersionConstraint = Just constraint
      , depType = BuildDepends 
      }

formatUpgrade :: Dependency -> Text
formatUpgrade dep = formatDependency dep
