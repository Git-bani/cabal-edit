{-# LANGUAGE OverloadedStrings #-}

module Golden.RoundtripSpec (spec) where

import Test.Hspec
import Business.Add
import Business.Remove
import Core.Types
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, copyFile, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)
import Control.Monad (forM_)

spec :: Spec
spec = describe "Golden Roundtrip" $ do
  
  let fixtures = ["complex.cabal", "common-stanzas.cabal"]
  
  forM_ fixtures $ \fixture -> do
    it ("preserves " ++ fixture ++ " exactly after Add + Remove cycle") $ do
      withFixture fixture $ \path -> do
        original <- TIO.readFile path
        
        -- 1. Add a unique dependency
        let addOpts = AddOptions 
              { aoPackageNames = ["containers"]
              , aoVersion = Just ">=0.1"
                          , aoSection = TargetLib
                          , aoDev = False
                          , aoDryRun = False
                          , aoGit = Nothing
                          , aoTag = Nothing
                          , aoPath = Nothing
                          }        
        resAdd <- addDependency Nothing addOpts path
        resAdd `shouldSatisfy` isSuccess
        
        -- 2. Remove the same dependency
        let rmOpts = RemoveOptions
              { roPackageNames = ["containers"]
              , roSection = TargetLib
              , roDryRun = False
              }
              
        resRm <- removeDependency rmOpts path
        resRm `shouldSatisfy` isSuccess
        
        -- 3. Verify content match
        final <- TIO.readFile path
        
        final `shouldBe` original

withFixture :: String -> (FilePath -> IO a) -> IO a
withFixture name action = do
  cwd <- getCurrentDirectory
  let fixturePath = cwd </> "test/Golden/fixtures" </> name
  let tempPath = cwd </> "test/Golden/fixtures" </> ("temp_" ++ name)
  let bakPath = tempPath ++ ".bak"
  
  bracket
    (copyFile fixturePath tempPath >> return tempPath)
    (\p -> do
        ignoringIOErrors $ removeFile p
        ignoringIOErrors $ removeFile bakPath
    )
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False
