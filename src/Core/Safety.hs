{-# LANGUAGE OverloadedStrings #-}

module Core.Safety
  ( withSafeWrite
  , safeWriteCabal
  , safeWriteFile
  , verifyCabalContent
  , createBackup
  ) where

import Core.Types
import Utils.Logging
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (copyFile, createDirectory, removeDirectory, renameFile, removeFile)
import System.FilePath ((<.>))
import Control.Exception (bracket, throwIO, catch, IOException, onException)
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import Data.Text.Encoding (encodeUtf8)

-- | Simple directory-based locking
withFileLock :: FilePath -> IO a -> IO a
withFileLock path action = do
  let lockPath = path <.> "lock"
  logDebug $ "Acquiring lock: " <> T.pack lockPath
  bracket (acquireLock lockPath 10)
          (\_ -> do
              logDebug $ "Releasing lock: " <> T.pack lockPath
              releaseLock lockPath)
          (\_ -> action)

acquireLock :: FilePath -> Int -> IO ()
acquireLock lockPath 0 = throwIO $ Error ("Could not acquire lock: " <> T.pack lockPath) FileModificationError
acquireLock lockPath retries = do
  result <- tryCreate lockPath
  case result of
    True -> return ()
    False -> do
      threadDelay 100000 -- 100ms
      acquireLock lockPath (retries - 1)

tryCreate :: FilePath -> IO Bool
tryCreate path = 
  catch (createDirectory path >> return True)
        (\e -> let _ = (e :: IOException) in return False)

releaseLock :: FilePath -> IO ()
releaseLock path = 
  catch (removeDirectory path)
        (\e -> let _ = (e :: IOException) in return ())

-- | Safely write to a cabal file with locking, backup, and verification
withSafeWrite :: FilePath -> Text -> (CabalFile -> IO CabalFile) -> IO (Result ())
withSafeWrite path originalContent _ = do
  withFileLock path $ do
    createBackup path
    currentContent <- TIO.readFile path
    if currentContent /= originalContent
      then return $ Failure $ Error "File changed on disk before write" FileModificationError
      else do
        return $ Failure $ Error "Not implemented fully yet" FileModificationError

-- | Better signature for the safety wrapper
safeWriteCabal :: FilePath -> Text -> IO (Result ())
safeWriteCabal path newContent = do
   case verifyCabalContent newContent of
     Left err -> return $ Failure $ Error ("Verification failed: " <> err) ParseError
     Right _ -> safeWriteFile path newContent

-- | Generic safe write (atomic + backup + locking) without Cabal verification
safeWriteFile :: FilePath -> Text -> IO (Result ())
safeWriteFile path newContent = do
  withFileLock path $ do
     createBackup path
     logDebug $ "Writing to file: " <> T.pack path
     
     let tempPath = path <.> "tmp"
     logDebug $ "Writing to temporary file: " <> T.pack tempPath
     
     let writeAction = do
           BS.writeFile tempPath (encodeUtf8 newContent)
           renameFile tempPath path
           return $ Success ()
           
     writeAction `onException` (ignoringIOErrors $ removeFile tempPath)

-- | Create a backup file
createBackup :: FilePath -> IO ()
createBackup path = do
  let backupPath = path <.> "bak"
  logDebug $ "Creating backup: " <> T.pack backupPath
  catch (copyFile path backupPath)
        (\e -> let _ = (e :: IOException) in return ())

-- | Verify that the content is a valid Cabal file
verifyCabalContent :: Text -> Either Text ()
verifyCabalContent content = 
  let bs = encodeUtf8 content
      parseRes = Cabal.runParseResult $ Cabal.parseGenericPackageDescription bs
  in case parseRes of
       (_, Right _) -> Right ()
       (_, Left err) -> Left (T.pack $ show err)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())
