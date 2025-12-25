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

spec :: Spec
spec = describe "Golden Roundtrip" $ do
  
  it "preserves file exactly after Add + Remove cycle" $ do
    withFixture "complex.cabal" $ \path -> do
      original <- TIO.readFile path
      
      -- 1. Add a unique dependency
      let addOpts = AddOptions 
            { aoPackageName = "containers"
            , aoVersion = Just ">=0.1"
            , aoSection = TargetLib
            , aoDev = False
            , aoDryRun = False
            }
      
      resAdd <- addDependency addOpts path
      resAdd `shouldSatisfy` isSuccess
      
      -- 2. Remove the same dependency
      let rmOpts = RemoveOptions
            { roPackageName = "containers"
            , roSection = TargetLib
            , roDryRun = False
            }
            
      resRm <- removeDependency rmOpts path
      resRm `shouldSatisfy` isSuccess
      
      -- 3. Verify content match
      final <- TIO.readFile path
      
      -- Note: There might be minor whitespace differences depending on how we insert/remove lines.
      -- Ideally, it should be identical.
      -- If insert adds a newline and remove deletes it including the newline, it should work.
      
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
