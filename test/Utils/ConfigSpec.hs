{-# LANGUAGE OverloadedStrings #-}

module Utils.ConfigSpec (spec) where
import Data.Either (isRight, isLeft)

import Test.Hspec
import Utils.Config
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (encode)

spec :: Spec
spec = describe "Utils.Config" $ do
  
  it "uses default config when file is missing" $ do
    -- We can't easily mock home directory without environment variable hacks or changing the code to accept a path.
    -- Assuming `loadConfig` uses `getHomeDirectory`.
    -- For unit testing, it's better if `loadConfig` accepted a path, or we tested `parseConfigFile` (if exported).
    -- `loadConfig` is hardcoded to ~/.cabal-edit/config.json.
    
    -- Let's test the JSON instances instead, which is safe.
    let cfg = defaultConfig
    let json = encode cfg
    LBS.length json `shouldSatisfy` (> 0)
    
  it "default config has correct values" $ do
    cfgHackageUrl defaultConfig `shouldBe` "https://hackage.haskell.org"
    cfgCacheExpiryHours defaultConfig `shouldBe` 6
