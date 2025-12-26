{-# LANGUAGE OverloadedStrings #-}

module Business.SetVersion (setVersion) where

import Core.Types (SetVersionOptions(..), Result(..), CabalFile(..))
import Core.Parser
import Core.Safety
import Utils.Logging (logInfo)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

setVersion :: SetVersionOptions -> FilePath -> IO (Result ())
setVersion opts path = do
  parseResult <- parseCabalFile path
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      let newVersion = svoVersion opts
      let oldContent = cfRawContent cabalFile
      let newContent = updateProjectVersion newVersion oldContent
      
      if newContent == oldContent
        then return $ Success ()
        else if svoDryRun opts
          then do
            logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
            TIO.putStrLn newContent
            return $ Success ()
          else safeWriteCabal path newContent

updateProjectVersion :: Text -> Text -> Text
updateProjectVersion newVer content =
  let ls = T.lines content
      newLines = map updateLine ls
  in T.unlines newLines
  where
    updateLine line =
      let trimmed = T.stripStart line
      in if "version:" `T.isPrefixOf` T.toLower trimmed
         then 
           let indent = T.takeWhile (== ' ') line
           in indent <> "version:            " <> newVer
         else line
