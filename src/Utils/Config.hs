{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Utils.Config
  ( Config(..)
  , loadConfig
  , defaultConfig
  ) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Control.Exception (try, SomeException)

data Config = Config
  { cfgHackageUrl :: Text
  , cfgCacheExpiryHours :: Int
  , cfgLeadingComma :: Bool
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "hackage_url" .!= "https://hackage.haskell.org"
    <*> v .:? "cache_expiry_hours" .!= 6
    <*> v .:? "leading_comma" .!= True

instance ToJSON Config where
  toJSON c = object
    [ "hackage_url" .= cfgHackageUrl c
    , "cache_expiry_hours" .= cfgCacheExpiryHours c
    , "leading_comma" .= cfgLeadingComma c
    ]

defaultConfig :: Config
defaultConfig = Config
  { cfgHackageUrl = "https://hackage.haskell.org"
  , cfgCacheExpiryHours = 6
  , cfgLeadingComma = True
  }

configDirName :: FilePath
configDirName = ".cabal-edit"

configFileName :: FilePath
configFileName = "config.json"

-- | Load config from ~/.cabal-edit/config.json
loadConfig :: IO Config
loadConfig = do
  home <- getHomeDirectory
  let configPath = home </> configDirName </> configFileName
  
  exists <- doesFileExist configPath
  if exists
    then do
      result <- try (eitherDecodeFileStrict configPath) :: IO (Either SomeException (Either String Config))
      case result of
        Left _ -> return defaultConfig -- IO Error
        Right (Left _) -> return defaultConfig -- Parse Error (TODO: Log this)
        Right (Right cfg) -> return cfg
    else do
      -- Create default config file
      createDefaultConfig configPath
      return defaultConfig

createDefaultConfig :: FilePath -> IO ()
createDefaultConfig path = do
  let dir = takeDirectory path -- use takeDirectory
  createDirectoryIfMissing True dir
  encodeFile path defaultConfig