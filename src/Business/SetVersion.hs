{-# LANGUAGE OverloadedStrings #-}

module Business.SetVersion (setVersion) where

import Core.Types (SetVersionOptions(..), Error)
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (updateFieldInAST)
import Core.Safety
import Utils.Logging (logInfo)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Utils.Diff (diffLines, colorizeDiff)

setVersion :: SetVersionOptions -> FilePath -> IO (Either Error ())
setVersion opts path = do
  content <- TIO.readFile path
  let ast = parseAST content
  
  case updateFieldInAST "version" (svoVersion opts) ast of
    Left err -> return $ Left err
    Right updatedAST -> do
      let newContent = serializeAST updatedAST
      if newContent == content
        then return $ Right ()
        else if svoDryRun opts
          then do
            logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
            let diffs = diffLines (T.lines content) (T.lines newContent)
            colorizeDiff diffs
            return $ Right ()
          else safeWriteFile path newContent