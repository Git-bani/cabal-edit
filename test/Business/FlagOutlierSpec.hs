{-# LANGUAGE OverloadedStrings #-}

module Business.FlagOutlierSpec (spec) where

import Test.Hspec
import Business.Flag
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Business.Flag (Outliers)" $ do
  
  it "matches flag names case-insensitively" $ do
    let content = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag MyFlag"
          , "    default: False"
          ]
    withTempCabalFile content $ \path -> do
      let opts = FlagOptions (Just "myflag") FlagEnable False False
      result <- handleFlag opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      T.unpack final `shouldContain` "default: True"
      T.unpack final `shouldContain` "flag MyFlag" -- Preserves original case

  it "preserves comments inside flag blocks when modifying" $ do
    let content = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag debug"
          , "    -- This is a comment"
          , "    default: False"
          , "    manual: True"
          ]
    withTempCabalFile content $ \path -> do
      let opts = FlagOptions (Just "debug") FlagEnable False False
      result <- handleFlag opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      T.unpack final `shouldContain` "-- This is a comment"
      T.unpack final `shouldContain` "default: True"

  it "handles boolean values case-insensitively (True/true)" $ do
    let content = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag debug"
          , "    default: false" -- lowercase
          ]
    withTempCabalFile content $ \path -> do
      let opts = FlagOptions (Just "debug") FlagEnable False False
      result <- handleFlag opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      T.unpack final `shouldContain` "default: True" -- Normalized to TitleCase by our editor, usually

  it "dry run does not modify file" $ do
    let content = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag debug", "    default: False"
          ]
    withTempCabalFile content $ \path -> do
      let opts = FlagOptions (Just "debug") FlagEnable True False -- DryRun = True
      result <- handleFlag opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      final `shouldBe` content

  it "fails gracefully when adding a flag that already exists" $ do
    let content = T.unlines ["cabal-version: 2.4", "name: test", "version: 0.1", "flag existing"]
    withTempCabalFile content $ \path -> do
      let opts = FlagOptions (Just "existing") FlagAdd False False
      result <- handleFlag opts path
      case result of
        Failure (Error msg InvalidDependency) -> T.unpack msg `shouldContain` "Flag already exists"
        _ -> expectationFailure $ "Expected failure, got " ++ show result

  it "fails gracefully when modifying a non-existent flag" $ do
    let content = T.unlines ["cabal-version: 2.4", "name: test", "version: 0.1"]
    withTempCabalFile content $ \path -> do
      let opts = FlagOptions (Just "missing") FlagEnable False False
      result <- handleFlag opts path
      case result of
        Failure (Error msg FileNotFound) -> T.unpack msg `shouldContain` "Flag not found"
        _ -> expectationFailure $ "Expected failure, got " ++ show result

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_flag_outlier.cabal"
  let bakPath = path ++ ".bak" 
  
  bracket 
    (TIO.writeFile path content >> return path)
    (\p -> do
        ignoringIOErrors $ removeFile p
        ignoringIOErrors $ removeFile bakPath
    ) 
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())
