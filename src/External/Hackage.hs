{-# LANGUAGE OverloadedStrings #-}

module External.Hackage
  ( fetchPackageVersions
  , fetchLatestVersion
  , searchPackages
  , fetchPackageMetadata
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
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)

-- | Search for packages on Hackage by keyword
searchPackages :: Text -> IO (Either Error [PackageMetadata])
searchPackages query = do
  cfg <- loadConfig
  let url = cfgHackageUrl cfg <> "/packages/search?terms=" <> query
  
  result <- httpGetJSON url :: IO (Either Error [HackageSearchResult])
  case result of
    Left err -> return $ Left err
    Right searchResults -> do
      -- To be efficient, we return what we can from search results
      -- then fetch detail only if needed, but for 'add -i' we want synopsis at least.
      -- Hackage search results sometimes have synopsis.
      return $ Right $ map (\r -> PackageMetadata 
        { pmName = hsrName r
        , pmSynopsis = fromMaybe "" (hsrSynopsis r)
        , pmLatestVersion = "" -- Not always in search
        , pmDownloads = Nothing
        , pmLicense = Nothing
        }) searchResults

data HackageSearchResult = HackageSearchResult 
  { hsrName :: Text 
  , hsrSynopsis :: Maybe Text
  }

instance FromJSON HackageSearchResult where
  parseJSON = withObject "HackageSearchResult" $ \v -> HackageSearchResult 
    <$> v .: "name"
    <*> v .:? "synopsis"

-- | Fetch detailed metadata for a package
fetchPackageMetadata :: Text -> IO (Either Error PackageMetadata)
fetchPackageMetadata pkgName = do
  cfg <- loadConfig
  let url = cfgHackageUrl cfg <> "/package/" <> pkgName <> ".json"
  -- Hackage .json endpoint for package details
  result <- httpGetJSON url :: IO (Either Error HackagePackageDetail)
  case result of
    Left err -> return $ Left err
    Right detail -> return $ Right $ PackageMetadata
      { pmName = pkgName
      , pmSynopsis = hpdSynopsis detail
      , pmLatestVersion = hpdLatestVersion detail
      , pmDownloads = Nothing -- Need separate API for downloads usually
      , pmLicense = Just (hpdLicense detail)
      }

data HackagePackageDetail = HackagePackageDetail
  { hpdSynopsis :: Text
  , hpdLatestVersion :: Text
  , hpdLicense :: Text
  }

instance FromJSON HackagePackageDetail where
  parseJSON = withObject "HackagePackageDetail" $ \v -> HackagePackageDetail
    <$> v .: "synopsis"
    <*> v .: "version"
    <*> v .: "license"

-- | Fetch latest version of a package
-- Uses Hackage JSON API: GET /package/{pkg} -> {"1.0.0":"normal", ...}
fetchLatestVersion :: Text -> IO (Either Error Version)
fetchLatestVersion pkgName = do
  cfg <- loadConfig
  
  -- 1. Check Cache
  cached <- checkCache cfg pkgName
  case cached of
    Just ver -> return $ Right ver
    Nothing -> do
      -- 2. Fetch Package Info
      let url = cfgHackageUrl cfg <> "/package/" <> pkgName
      result <- httpGetJSON url :: IO (Either Error (Map Text Text))
      
      case result of
        Left err -> return $ Left err
        Right versionMap -> do
          -- 3. Parse Map
          -- Filter for "normal" versions
          let validVersions = Map.filter (== "normal") versionMap
          -- Parse keys to Version
          let versions = map parseVersionText (Map.keys validVersions)
          -- Get Maximum
          case versions of
            [] -> return $ Left $ Error ("No valid versions found for: " <> pkgName) InvalidDependency
            vs -> do
              let latest = maximum vs
              saveCache pkgName latest
              return $ Right latest

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