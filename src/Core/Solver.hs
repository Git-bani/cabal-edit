{-# LANGUAGE OverloadedStrings #-}
module Core.Solver (verifyChanges) where

import Core.Types (Error(..), ErrorCode(..))
import Utils.Logging (logInfo, logError, logDebug, logWarning)
import System.Directory (renameFile, copyFile, doesFileExist)
import Control.Exception (bracket, catch, SomeException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

-- | Verifies changes by running the Cabal solver (dry-run) on a temporary version of the file.
-- This function modifies the file system (temporarily) to run 'cabal build --dry-run'.
verifyChanges :: FilePath -> T.Text -> IO (Either Error ())
verifyChanges path newContent = do
  logInfo "Running Cabal solver to verify changes..."
  
  let backupPath = path <> ".bak"
  
  -- We wrap the entire operation in a bracket to ensure restoration
  result <- tryBracket 
    (backupFile path backupPath) 
    (\_ -> restoreFile path backupPath) 
    (\_ -> do
       -- Write new content to the actual file location so 'cabal' picks it up
       TIO.writeFile path newContent
       
       logDebug "Executing: cabal build --dry-run"
       -- Run cabal build --dry-run
       -- Note: This assumes 'cabal' is in PATH and we are in a valid project context.
       -- If the command fails to run (e.g. cabal not found), catch it.
       (exitCode, stdout, stderr) <- catch 
           (readProcessWithExitCode "cabal" ["build", "--dry-run"] "")
           (\e -> return (ExitFailure 1, "", "Failed to execute cabal: " ++ show (e :: SomeException)))
       
       case exitCode of
         ExitSuccess -> do
           logInfo "Solver check passed: Configuration is valid."
           return $ Right ()
         ExitFailure _ -> do
           logError "Solver check failed!"
           -- Filter relevant error lines from stderr/stdout if possible?
           -- For now, just dump them.
           logError "--- Cabal Output ---"
           mapM_ (logError . T.pack) (lines stderr)
           mapM_ (logError . T.pack) (lines stdout)
           logError "--------------------"
           return $ Left $ Error "Cabal solver failed to resolve dependencies with proposed changes." VersionConflict
    )
  
  return result

-- | Helper for bracket that catches exceptions during the 'use' phase
tryBracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
tryBracket create destroy use = bracket create destroy use

backupFile :: FilePath -> FilePath -> IO ()
backupFile src dst = do
  exists <- doesFileExist src
  if exists 
    then copyFile src dst 
    else do
      logWarning $ "Original file " <> T.pack src <> " does not exist, cannot backup."
      -- This is a weird state, but if we proceed, restore might fail.
      -- If src doesn't exist, maybe we are creating a new file?
      -- But usually we are editing.
      return ()

restoreFile :: FilePath -> FilePath -> IO ()
restoreFile src backup = do
  exists <- doesFileExist backup
  if exists 
    then do
      renameFile backup src -- Overwrite modified file with backup
      logDebug "Restored original file."
    else do
       -- If backup doesn't exist, maybe we shouldn't have restored?
       -- Or maybe we should delete the 'src' if it was new?
       -- For safety, if backup missing, we do nothing and warn.
       existsSrc <- doesFileExist src
       if existsSrc
         then logWarning "Backup file missing, keeping modified file (potentially unsafe state)."
         else logDebug "No file to restore."
