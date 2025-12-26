{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Business.Flag (handleFlag) where

import Core.Types
import Core.Parser
import Core.Safety
import Utils.Logging (logInfo)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (find)

handleFlag :: FlagOptions -> FilePath -> IO (Result ())
handleFlag opts path = do
  parseResult <- parseCabalFile path
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      let name = foFlagName opts
      let sections = cfSections cabalFile
      let existingFlag = find (\case
                                 FlagSection f -> flagName f == name
                                 _ -> False) sections
      
      case (foOperation opts, existingFlag) of
        (FlagAdd, Just _) -> return $ Failure $ Error ("Flag already exists: " <> name) InvalidDependency
        (FlagAdd, Nothing) -> addFlag opts cabalFile path
        (FlagEnable, Just (FlagSection f)) -> setFlagDefault True f cabalFile path opts
        (FlagDisable, Just (FlagSection f)) -> setFlagDefault False f cabalFile path opts
        (FlagRemove, Just (FlagSection f)) -> removeFlag f cabalFile path opts
        (_, Nothing) -> return $ Failure $ Error ("Flag not found: " <> name) FileNotFound
        (_, _) -> return $ Failure $ Error "Internal error: invalid flag match" ParseError

addFlag :: FlagOptions -> CabalFile -> FilePath -> IO (Result ())
addFlag opts cabalFile path = do
  let name = foFlagName opts
      newStanza = T.unlines
        [ ""
        , "flag " <> name
        , "    description: " <> name
        , "    manual: True"
        , "    default: False"
        ]
      -- Insert flags before the first section or at end of header
      -- For simplicity, append after the package name/version/etc.
      oldContent = cfRawContent cabalFile
      ls = T.lines oldContent
      (header, rest) = span (\l -> not ("library" `T.isInfixOf` l || "executable" `T.isInfixOf` l || "common" `T.isInfixOf` l || "test-suite" `T.isInfixOf` l || "benchmark" `T.isInfixOf` l || "flag" `T.isInfixOf` l)) ls
      newContent = T.unlines header <> newStanza <> T.unlines rest
  
  writeOrDryRun opts path newContent

setFlagDefault :: Bool -> FlagStanza -> CabalFile -> FilePath -> FlagOptions -> IO (Result ())
setFlagDefault val stanza cabalFile path opts = do
  let TextSpan (TextOffset start) (TextOffset end) = flagPosition stanza
      oldContent = cfRawContent cabalFile
      (before, rest) = T.splitAt start oldContent
      (sectionContent, after) = T.splitAt (end - start) rest
      
      newSectionContent = updateDefaultField val sectionContent
      newContent = before <> newSectionContent <> after
  
  writeOrDryRun opts path newContent

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
      -- Remove potential trailing newlines
      newContent = before <> T.dropWhile (== '\n') after
  
  writeOrDryRun opts path newContent

writeOrDryRun :: FlagOptions -> FilePath -> Text -> IO (Result ())
writeOrDryRun opts path newContent = 
  if foDryRun opts
    then do
      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
      TIO.putStrLn newContent
      return $ Success ()
    else safeWriteCabal path newContent
