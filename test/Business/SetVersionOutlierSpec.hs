{-# LANGUAGE OverloadedStrings #-}

module Business.SetVersionOutlierSpec (spec) where

import Test.Hspec
import Business.SetVersion
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Business.SetVersion (Outliers)" $ do

  it "preserves surrounding comments and whitespace" $ do
    let content = T.unlines
          [ "cabal-version: 2.4"
          , "-- Top comment"
          , ""
          , "name: test"
          , "-- Version comment"
          , "version: 0.1"
          , ""
          , "library"
          ]
    withTempCabalFile content $ \path -> do
      let opts = SetVersionOptions { svoVersion = "0.2", svoDryRun = False }
      result <- setVersion opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      T.unpack final `shouldContain` "-- Top comment"
      T.unpack final `shouldContain` "-- Version comment"
      T.unpack final `shouldContain` "version: 0.2"

  it "handles case-insensitive field names" $ do
    let content = T.unlines
          [ "cabal-version: 2.4"
          , "Name: test"
          , "VERSION: 0.1" -- Uppercase
          ]
    withTempCabalFile content $ \path -> do
      let opts = SetVersionOptions { svoVersion = "0.2", svoDryRun = False }
      result <- setVersion opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      T.unpack final `shouldContain` "VERSION: 0.2" -- Should preserve case of field key? Or normalize?
      -- The editor typically preserves the key if it just updates value.

  it "dry run does not modify file" $ do
    let content = T.unlines ["cabal-version: 2.4", "name: test", "version: 0.1"]
    withTempCabalFile content $ \path -> do
      let opts = SetVersionOptions { svoVersion = "0.2", svoDryRun = True }
      result <- setVersion opts path
      result `shouldSatisfy` isSuccess
      
      final <- TIO.readFile path
      final `shouldBe` content

  it "fails if version field is missing (current behavior)" $ do
    -- Ideally it should add it, but currently updateFieldInAST fails if not found.
    -- Let's verify this behavior.
    let content = T.unlines ["cabal-version: 2.4", "name: test"]
    withTempCabalFile content $ \path -> do
      let opts = SetVersionOptions { svoVersion = "0.2", svoDryRun = False }
      result <- setVersion opts path
      case result of
        Failure (Error msg ParseError) -> T.unpack msg `shouldContain` "Field not found"
        _ -> expectationFailure $ "Expected failure, got " ++ show result

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_setversion_outlier.cabal"
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
