{-# LANGUAGE OverloadedStrings #-}

module Business.Upgrade (upgradeDependencies) where

import Core.Types
import Core.Parser
import Core.Serializer
import Core.Safety
import Core.DependencyResolver
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortOn)
import Data.Ord (Down(..))
import Control.Monad (foldM)

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
      
      -- 2. Sort sections by position descending (to safely modify from end to start)
      let sortedSections = sortOn (Down . getStartOffset) sections
      
      -- 3. Apply upgrades section by section
      finalContentResult <- foldM (processSection opts (cfLineEndings cabalFile) leadingComma) (Success rawContent) sortedSections
      
      case finalContentResult of
        Failure err -> return $ Failure err
        Success newContent -> do
          -- 4. Write back if changed
          if newContent /= rawContent
            then 
              if uoDryRun opts
                then do
                  logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                  TIO.putStrLn newContent
                  return $ Success ()
                else safeWriteCabal path newContent
            else return $ Success ()

getStartOffset :: Section -> Int
getStartOffset s = 
  let TextSpan (TextOffset start) _ = getSectionBounds s 
  in start

processSection :: UpgradeOptions -> Text -> Bool -> Result Text -> Section -> IO (Result Text)
processSection _ _ _ (Failure err) _ = return $ Failure err
processSection opts eol leadingComma (Success content) section = do
  let deps = findDependencies section
  
  -- Filter dependencies if user requested specific package
  let targetResult = case uoPackageName opts of
                       Nothing -> Success deps
                       Just name -> case mkPackageName name of
                                      Left err -> Failure $ Error err InvalidDependency
                                      Right pkgName -> Success $ filter (\d -> depName d == pkgName) deps
  
  case targetResult of
    Failure err -> return $ Failure err
    Success targetDeps -> do
      if null targetDeps
        then return $ Success content
        else do
          -- Resolve latest versions for these dependencies
          upgradedDepsResult <- mapM upgradeDependency targetDeps
          
          -- Check if any failed
          let failures = [e | Failure e <- upgradedDepsResult]
          case failures of
            (e:_) -> return $ Failure e -- Return first error
            [] -> do
              let upgrades = [d | Success d <- upgradedDepsResult]
              
              -- Merge upgrades into original list
              -- We need to replace items in 'deps' with those in 'upgrades'
              let newDeps = mergeDependencies deps upgrades
              
              -- Apply change to content
              if newDeps == deps
                then return $ Success content -- No changes needed
                else return $ Success $ applySectionUpdate eol leadingComma content section newDeps

upgradeDependency :: Dependency -> IO (Result Dependency)
upgradeDependency dep = do
  -- Note: resolveLatestVersion now expects PackageName
  res <- resolveLatestVersion (depName dep)
  case res of
    Failure err -> return $ Failure err
    Success ver -> do
      -- Create new dependency with ExactVersion
      let newConstraint = ExactVersion ver
      -- Check if it's different from current
      -- (Simplified check, ideally we check if current satisfies latest, but 'upgrade' usually forces latest)
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
      
      -- Replace the block in the section content
      newSectionContent = replaceBuildDependsBlock eol leadingComma newDeps sectionContent
  in before <> newSectionContent <> after
