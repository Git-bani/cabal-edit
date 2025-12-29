{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Business.Flag (handleFlag) where

import Core.Types
import Core.Parser
import Core.Safety
import Utils.Logging (logInfo)
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import Data.List (find)

import Utils.Terminal (toggleDashboard)
import Utils.Diff (diffLines, colorizeDiff)
import Control.Monad (foldM)

handleFlag :: FlagOptions -> FilePath -> IO (Result ())
handleFlag opts path = do
  parseResult <- parseCabalFile path
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      if foInteractive opts
        then runFlagDashboard cabalFile path opts
        else handleSingleFlag cabalFile path opts

runFlagDashboard :: CabalFile -> FilePath -> FlagOptions -> IO (Result ())
runFlagDashboard cabalFile path opts = do
  let sections = cfSections cabalFile
      flags = [f | FlagSection f <- sections]
  
  if null flags
    then return $ Failure $ Error "No flags found in project" FileNotFound
    else do
      let itemsWithStates = [(flagName f, flagDefault f) | f <- flags]
      finalItemsWithStates <- toggleDashboard "Flag Dashboard" itemsWithStates
      
      -- Apply changes
      finalContentResult <- foldM (applyFlagToggle flags) (Success $ cfRawContent cabalFile) finalItemsWithStates
      
      case finalContentResult of
        Failure err -> return $ Failure err
        Success finalContent -> writeOrDryRun opts path (cfRawContent cabalFile) finalContent

applyFlagToggle :: [FlagStanza] -> Result Text -> (Text, Bool) -> IO (Result Text)
applyFlagToggle _ (Failure err) _ = return $ Failure err
applyFlagToggle stanzas (Success currentContent) (name, newVal) = do
  case find (\f -> flagName f == name) stanzas of
    Nothing -> return $ Success currentContent -- Should not happen
    Just stanza -> do
      if flagDefault stanza == newVal
        then return $ Success currentContent
        else do
          let TextSpan (TextOffset start) (TextOffset end) = flagPosition stanza
              (before, rest) = T.splitAt start currentContent
              (sectionContent, after) = T.splitAt (end - start) rest
              newSectionContent = updateDefaultField newVal sectionContent
          return $ Success $ before <> newSectionContent <> after

handleSingleFlag :: CabalFile -> FilePath -> FlagOptions -> IO (Result ())
handleSingleFlag cabalFile path opts = do
  case foFlagName opts of
    Nothing -> return $ Failure $ Error "Flag name required for non-interactive mode" InvalidDependency
    Just name -> do
      let sections = cfSections cabalFile
          existingFlag = find (\case
                                 FlagSection f -> flagName f == name
                                 _ -> False) sections
      
      case (foOperation opts, existingFlag) of
        (FlagAdd, Just _) -> return $ Failure $ Error ("Flag already exists: " <> name) InvalidDependency
        (FlagAdd, Nothing) -> addFlag name opts cabalFile path
        (FlagEnable, Just (FlagSection f)) -> setFlagDefault True f cabalFile path opts
        (FlagDisable, Just (FlagSection f)) -> setFlagDefault False f cabalFile path opts
        (FlagRemove, Just (FlagSection f)) -> removeFlag f cabalFile path opts
        (_, Nothing) -> return $ Failure $ Error ("Flag not found: " <> name) FileNotFound
        (_, _) -> return $ Failure $ Error "Internal error: invalid flag match" ParseError

addFlag :: Text -> FlagOptions -> CabalFile -> FilePath -> IO (Result ())
addFlag name opts cabalFile path = do
  let newStanza = T.unlines
        [ ""
        , "flag " <> name
        , "    description: " <> name
        , "    manual: True"
        , "    default: False"
        ]
      oldContent = cfRawContent cabalFile
      ls = T.lines oldContent
      (header, rest) = span (\l -> not ("library" `T.isInfixOf` l || "executable" `T.isInfixOf` l || "common" `T.isInfixOf` l || "test-suite" `T.isInfixOf` l || "benchmark" `T.isInfixOf` l || "flag" `T.isInfixOf` l)) ls
      newContent = T.unlines header <> newStanza <> T.unlines rest
  
  writeOrDryRun opts path oldContent newContent

setFlagDefault :: Bool -> FlagStanza -> CabalFile -> FilePath -> FlagOptions -> IO (Result ())
setFlagDefault val stanza cabalFile path opts = do
  let TextSpan (TextOffset start) (TextOffset end) = flagPosition stanza
      oldContent = cfRawContent cabalFile
      (before, rest) = T.splitAt start oldContent
      (sectionContent, after) = T.splitAt (end - start) rest
      
      newSectionContent = updateDefaultField val sectionContent
      newContent = before <> newSectionContent <> after
  
  writeOrDryRun opts path oldContent newContent

updateDefaultField :: Bool -> Text -> Text
updateDefaultField val content = 
  let ls = T.lines content
      valStr = if val then "True" else "False"
      newLines = map (\l -> if "default:" `T.isPrefixOf` T.toLower (T.stripStart l)
                            then (T.takeWhile (== ' ') l) <> "default: " <> valStr
                            else l) ls
  in T.unlines newLines

removeFlag :: FlagStanza -> CabalFile -> FilePath -> FlagOptions -> IO (Result ())
removeFlag stanza cabalFile path opts = do
  let TextSpan (TextOffset start) (TextOffset end) = flagPosition stanza
      oldContent = cfRawContent cabalFile
      (before, rest) = T.splitAt start oldContent
      (_, after) = T.splitAt (end - start) rest
      newContent = before <> T.dropWhile (== '\n') after
  
  writeOrDryRun opts path oldContent newContent

writeOrDryRun :: FlagOptions -> FilePath -> Text -> Text -> IO (Result ())
writeOrDryRun opts path oldContent newContent = 
  if foDryRun opts
    then do
      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
      let diffs = diffLines (T.lines oldContent) (T.lines newContent)
      colorizeDiff diffs
      return $ Success ()
    else safeWriteCabal path newContent

