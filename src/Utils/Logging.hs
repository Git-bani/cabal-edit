{-# LANGUAGE OverloadedStrings #-}

module Utils.Logging
  ( LogLevel(..)
  , setLogLevel
  , logInfo
  , logWarning
  , logError
  , logDebug
  , logSuccess
  , colorize
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (stderr, hIsTerminalDevice)
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data LogLevel = Debug | Info | WarnLevel | SuccessLevel | ErrorLevel | Quiet
  deriving (Eq, Ord, Show)

-- | Global log level state
{-# NOINLINE logLevelRef #-}
logLevelRef :: IORef LogLevel
logLevelRef = unsafePerformIO $ newIORef Info

setLogLevel :: LogLevel -> IO ()
setLogLevel = writeIORef logLevelRef

getLogLevel :: IO LogLevel
getLogLevel = readIORef logLevelRef

logMsg :: LogLevel -> Text -> IO ()
logMsg level msg = do
  minLevel <- getLogLevel
  when (level >= minLevel && minLevel /= Quiet) $ do
    isTTY <- hIsTerminalDevice stderr
    let maybeColor code txt = if isTTY then colorize code txt else txt
    let prefix = case level of
          Debug        -> maybeColor "36" "[DEBUG] "
          WarnLevel    -> maybeColor "33" "[WARNING] "
          ErrorLevel   -> maybeColor "31" "Error: "
          SuccessLevel -> maybeColor "32" "✓ "
          _            -> ""
    TIO.hPutStrLn stderr (prefix <> msg)

logInfo :: Text -> IO ()
logInfo = logMsg Info

logWarning :: Text -> IO ()
logWarning = logMsg WarnLevel

logError :: Text -> IO ()
logError = logMsg ErrorLevel

logDebug :: Text -> IO ()
logDebug = logMsg Debug

logSuccess :: Text -> IO ()
logSuccess = logMsg SuccessLevel

colorize :: Text -> Text -> Text
colorize code txt = "\x1b[" <> code <> "m" <> txt <> "\x1b[0m"