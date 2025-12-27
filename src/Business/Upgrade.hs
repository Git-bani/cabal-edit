{-# LANGUAGE OverloadedStrings #-}

module Business.Upgrade (upgradeDependencies) where

import Core.Types
import Core.Parser
import Core.Serializer (replaceBuildDependsBlock, formatVersionConstraint)
import Core.Safety
import Core.DependencyResolver
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import Utils.Terminal (selectItems)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortOn)
import Data.Ord (Down(..))
import Control.Monad (foldM, forM)

import Utils.Diff (diffLines, colorizeDiff)

upgradeDependencies :: UpgradeOptions -> FilePath -> IO (Result ())
upgradeDependencies opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  -- 1. Parse cabal file
  parseResult <- parseCabalFile path
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      let rawContent = cfRawContent cabalFile
      let sections = cfSections cabalFile
      
      -- 2. Gathering all upgradeable dependencies across all sections
      allUpgrades <- gatherAllUpgrades opts sections
      
      case allUpgrades of
        Failure err -> return $ Failure err
        Success potentialUpgrades -> do
          -- 3. Handle interactive selection
          selectedUpgrades <- if uoInteractive opts && not (null potentialUpgrades)
            then do
              let items = map formatUpgrade potentialUpgrades
              selectedItems <- selectItems "Select packages to upgrade:" items
              return $ filter (\u -> formatUpgrade u `elem` selectedItems) potentialUpgrades
            else return potentialUpgrades
          
          if null selectedUpgrades
            then return $ Success ()
            else do
              -- 4. Sort sections by position descending (to safely modify from end to start)
              let sortedSections = sortOn (Down . getStartOffset) sections
              
              -- 5. Apply selected upgrades section by section
              finalContentResult <- foldM (processSection selectedUpgrades (cfLineEndings cabalFile) leadingComma) (Success rawContent) sortedSections
              
              case finalContentResult of
                Failure err -> return $ Failure err
                Success newContent -> do
                  -- 6. Write back if changed
                  if newContent /= rawContent
                    then 
                      if uoDryRun opts
                        then do
                          logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                          let diffs = diffLines (T.lines rawContent) (T.lines newContent)
                          colorizeDiff diffs
                          return $ Success ()
                        else safeWriteCabal path newContent
                    else return $ Success ()

getStartOffset :: Section -> Int
getStartOffset s = 
  let TextSpan (TextOffset start) _ = getSectionBounds s 
  in start

gatherAllUpgrades :: UpgradeOptions -> [Section] -> IO (Result [Dependency])
gatherAllUpgrades opts sections = do
  let allDeps = concatMap findDependencies sections
  let targetDeps = if null (uoPackageNames opts)
                   then allDeps
                   else 
                     let pkgNames = map unsafeMkPackageName (uoPackageNames opts)
                     in filter (\d -> depName d `elem` pkgNames) allDeps
  
  -- Resolve latest versions
  upgradedDepsResult <- forM targetDeps upgradeDependency
  let failures = [e | Failure e <- upgradedDepsResult]
  case failures of
    (e:_) -> return $ Failure e
    [] -> return $ Success [d | Success d <- upgradedDepsResult]

formatUpgrade :: Dependency -> Text
formatUpgrade dep = unPackageName (depName dep) <> " " <> formatVersionConstraint (depVersionConstraint dep)

processSection :: [Dependency] -> Text -> Bool -> Result Text -> Section -> IO (Result Text)
processSection _ _ _ (Failure err) _ = return $ Failure err
processSection selectedUpgrades eol leadingComma (Success content) section = do
  let deps = findDependencies section
  -- Find upgrades that apply to this section
  let upgradesForThisSection = filter (\u -> any (\d -> depName d == depName u) deps) selectedUpgrades
  
  if null upgradesForThisSection
    then return $ Success content
    else 
      let newDeps = mergeDependencies deps upgradesForThisSection
      in if newDeps == deps
         then return $ Success content
         else return $ Success $ applySectionUpdate eol leadingComma content section newDeps

upgradeDependency :: Dependency -> IO (Result Dependency)
upgradeDependency dep = do
  res <- resolveLatestVersion (depName dep)
  case res of
    Failure err -> return $ Failure err
    Success ver -> do
      let newConstraint = MajorBoundVersion ver
      return $ Success $ dep { depVersionConstraint = Just newConstraint }

mergeDependencies :: [Dependency] -> [Dependency] -> [Dependency]
mergeDependencies original updates = 
  map (\d -> case findUpdate updates d of
               Just u -> u
               Nothing -> d
      ) original
  where
    findUpdate [] _ = Nothing
    findUpdate (u:us) d = if depName u == depName d then Just u else findUpdate us d

applySectionUpdate :: Text -> Bool -> Text -> Section -> [Dependency] -> Text
applySectionUpdate eol leadingComma content section newDeps =
  let TextSpan (TextOffset start) (TextOffset end) = getSectionBounds section
      (before, rest) = T.splitAt start content
      (sectionContent, after) = T.splitAt (end - start) rest
      
      newSectionContent = replaceBuildDependsBlock eol leadingComma newDeps sectionContent
  in before <> newSectionContent <> after