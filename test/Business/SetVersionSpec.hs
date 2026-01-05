{-# LANGUAGE OverloadedStrings #-}

module Business.SetVersionSpec (spec) where

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
spec = describe "Business.SetVersion" $ do
  
  it "updates the project version" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = SetVersionOptions 
            { svoVersion = "1.2.3.4"
            , svoDryRun = False
            }
      
      result <- setVersion opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "version: 1.2.3.4"
      T.unpack content `shouldNotContain` "version:            0.1.0.0"

basicCabalFile :: Text
basicCabalFile = T.unlines
  [ "cabal-version:      2.4"
  , "name:               test-project"
  , "version:            0.1.0.0"
  , ""
  , "library"
  , "    exposed-modules:  MyLib"
  ]

-- Helpers

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_set_version_spec.cabal"
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
