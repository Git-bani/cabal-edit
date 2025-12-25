{-# LANGUAGE OverloadedStrings #-}

module External.Network
  ( httpGet
  , httpGetJSON
  , withRetry
  ) where

import Core.Types
import Utils.Logging
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple
import Control.Exception (try, SomeException, displayException)
import Data.Aeson (FromJSON, eitherDecode)
import Control.Concurrent (threadDelay)

-- | Perform HTTP GET and return the raw body
httpGet :: Text -> IO (Either Error LBS.ByteString)
httpGet url = withRetry 3 $ do
  logDebug $ "HTTP GET: " <> url
  request <- parseRequest (T.unpack url)
  let request' = setRequestHeader "User-Agent" ["cabal-edit/0.1.0.0 (contact: anithabarns@gmail.com)"]
               $ setRequestHeader "Accept" ["application/json"] 
               request
  
  result <- try $ httpLBS request'
  case result of
    Left e -> 
      return $ Left $ Error (T.pack $ displayException (e :: SomeException)) NetworkError
    Right response -> 
      let status = getResponseStatusCode response
      in if status >= 200 && status < 300
         then do
           logDebug $ "HTTP Success: " <> T.pack (show status)
           return $ Right $ getResponseBody response
         else do
           logDebug $ "HTTP Failed: " <> T.pack (show status)
           return $ Left $ Error ("HTTP Error: " <> T.pack (show status)) NetworkError

-- | Perform HTTP GET and parse JSON
httpGetJSON :: FromJSON a => Text -> IO (Either Error a)
httpGetJSON url = do
  result <- httpGet url
  case result of
    Left err -> return $ Left err
    Right body -> case eitherDecode body of
      Left decodeErr -> do
          logDebug $ "JSON Decode Error Body: " <> T.pack (show body)
          return $ Left $ Error ("JSON Decode Error: " <> T.pack decodeErr) ParseError
      Right value -> return $ Right value

-- | Retry an IO action with exponential backoff
withRetry :: Int -> IO (Either Error a) -> IO (Either Error a)
withRetry 0 action = action
withRetry retries action = do
  result <- action
  case result of
    Right val -> return $ Right val
    Left err -> do
      if errorCode err == NetworkError
        then do
          let delay = 100000 * (4 - retries)
          logDebug $ "Retrying in " <> T.pack (show delay) <> "μs... (" <> T.pack (show retries) <> " left)"
          threadDelay delay
          withRetry (retries - 1) action
        else return $ Left err