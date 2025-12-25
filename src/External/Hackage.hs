{-# LANGUAGE OverloadedStrings #-}

module External.Hackage
  ( fetchPackageVersions
  , fetchLatestVersion
  ) where

import Core.Types
import External.Network
import Utils.Config (loadConfig, Config(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.FilePath ((</>))
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- | Fetch latest version of a package
-- Uses Hackage JSON API: GET /package/{pkg} -> {"1.0.0":"normal", ...}
fetchLatestVersion :: Text -> IO (Result Version)
fetchLatestVersion pkgName = do
  cfg <- loadConfig
  
  -- 1. Check Cache
  cached <- checkCache cfg pkgName
  case cached of
    Just ver -> return $ Success ver
    Nothing -> do
      -- 2. Fetch Package Info
      let url = cfgHackageUrl cfg <> "/package/" <> pkgName
      result <- httpGetJSON url :: IO (Either Error (Map Text Text))
      
      case result of
        Left err -> return $ Failure err
        Right versionMap -> do
          -- 3. Parse Map
          -- Filter for "normal" versions
          let validVersions = Map.filter (== "normal") versionMap
          -- Parse keys to Version
          let versions = map parseVersionText (Map.keys validVersions)
          -- Get Maximum
          case versions of
            [] -> return $ Failure $ Error ("No valid versions found for: " <> pkgName) InvalidDependency
            vs -> do
              let latest = maximum vs
              saveCache pkgName latest
              return $ Success latest

-- | Fetch all versions
fetchPackageVersions :: Text -> IO [Version]
fetchPackageVersions pkgName = do
  cfg <- loadConfig
  let url = cfgHackageUrl cfg <> "/package/" <> pkgName
  result <- httpGetJSON url :: IO (Either Error (Map Text Text))
  case result of
    Right versionMap -> do
       let validVersions = Map.filter (== "normal") versionMap
       return $ map parseVersionText (Map.keys validVersions)
    Left _ -> return []

-- | Parse version text "1.2.3" -> Version [1,2,3]
parseVersionText :: Text -> Version
parseVersionText t = 
  let parts = T.splitOn "." (T.strip t)
      nums = mapMaybe (readMaybe . T.unpack) parts
  in Version nums

--------------------------------------------------------------------------------
-- Caching Logic
--------------------------------------------------------------------------------

cacheDirName :: FilePath
cacheDirName = ".cabal-edit/cache"

getCachePath :: Text -> IO FilePath
getCachePath pkgName = do
  home <- getHomeDirectory
  let dir = home </> cacheDirName
  createDirectoryIfMissing True dir
  return $ dir </> ("latest_" <> T.unpack pkgName <> ".txt")

checkCache :: Config -> Text -> IO (Maybe Version)
checkCache cfg pkgName = do
  path <- getCachePath pkgName
  exists <- doesFileExist path
  if exists
    then do
      modTime <- getModificationTime path
      now <- getCurrentTime
      let expiry = fromIntegral (cfgCacheExpiryHours cfg) * 3600 -- hours to seconds
      if diffUTCTime now modTime < expiry
        then do
          content <- readFile path
          return $ Just $ parseVersionText (T.pack content)
        else return Nothing
    else return Nothing

saveCache :: Text -> Version -> IO ()
saveCache pkgName (Version nums) = do
  path <- getCachePath pkgName
  let verStr = T.intercalate "." (map (T.pack . show) nums)
  writeFile path (T.unpack verStr)