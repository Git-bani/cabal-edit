{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Business.Flag (handleFlag) where

import Core.Types
import Core.AST.Types (CabalAST)
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor
import Core.Safety
import Utils.Logging (logInfo)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (find)

import Utils.Terminal (toggleDashboard)
import Utils.Diff (diffLines, colorizeDiff)
import Control.Monad (foldM)

handleFlag :: FlagOptions -> FilePath -> IO (Either Error ())
handleFlag opts path = do
  content <- TIO.readFile path
  let ast = parseAST content
  if foInteractive opts
    then runFlagDashboard ast path opts
    else handleSingleFlag ast path opts

runFlagDashboard :: CabalAST -> FilePath -> FlagOptions -> IO (Either Error ())
runFlagDashboard ast path opts = do
  let flags = findFlagStanzasInAST ast
  
  if null flags
    then return $ Left $ Error "No flags found in project" FileNotFound
    else do
      finalItemsWithStates <- toggleDashboard "Flag Dashboard" flags
      
      -- Apply changes
      let finalASTResult = foldM applyFlagToggle ast finalItemsWithStates
            where
              applyFlagToggle currentAst (name, newVal) =
                case find (\(n, _) -> n == name) flags of
                  Just (_, oldVal) | oldVal /= newVal -> updateFlagDefaultInAST name newVal currentAst
                  _ -> Right currentAst
      
      case finalASTResult of
        Left err -> return $ Left err
        Right finalAST -> 
          let newContent = serializeAST finalAST
              oldContent = serializeAST ast
          in writeOrDryRun opts path oldContent newContent

handleSingleFlag :: CabalAST -> FilePath -> FlagOptions -> IO (Either Error ())
handleSingleFlag ast path opts = do
  case foFlagName opts of
    Nothing -> return $ Left $ Error "Flag name required for non-interactive mode" InvalidDependency
    Just name -> do
      let flags = findFlagStanzasInAST ast
          existing = find (\(n, _) -> T.toLower n == T.toLower name) flags
      
      case (foOperation opts, existing) of
        (FlagAdd, Just _) -> return $ Left $ Error ("Flag already exists: " <> name) InvalidDependency
        (FlagAdd, Nothing) -> 
          case addFlagToAST name ast of
            Left err -> return $ Left err
            Right newAST -> writeOrDryRun opts path (serializeAST ast) (serializeAST newAST)
        (FlagEnable, Just _) -> 
          case updateFlagDefaultInAST name True ast of
            Left err -> return $ Left err
            Right newAST -> writeOrDryRun opts path (serializeAST ast) (serializeAST newAST)
        (FlagDisable, Just _) -> 
          case updateFlagDefaultInAST name False ast of
            Left err -> return $ Left err
            Right newAST -> writeOrDryRun opts path (serializeAST ast) (serializeAST newAST)
        (FlagRemove, Just _) -> 
          case removeSectionFromAST "flag" name ast of
            Left err -> return $ Left err
            Right newAST -> writeOrDryRun opts path (serializeAST ast) (serializeAST newAST)
        (FlagEnable, Nothing) -> return $ Left $ Error ("Flag not found: " <> name) FileNotFound
        (FlagDisable, Nothing) -> return $ Left $ Error ("Flag not found: " <> name) FileNotFound
        (FlagRemove, Nothing) -> return $ Left $ Error ("Flag not found: " <> name) FileNotFound

writeOrDryRun :: FlagOptions -> FilePath -> Text -> Text -> IO (Either Error ())
writeOrDryRun opts path oldContent newContent = 
  if newContent == oldContent
    then return $ Right ()
    else if foDryRun opts
      then do
        logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
        let diffs = diffLines (T.lines oldContent) (T.lines newContent)
        colorizeDiff diffs
        return $ Right ()
      else safeWriteFile path newContent