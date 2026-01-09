{-# LANGUAGE OverloadedStrings #-}
module Business.DebugAddSpec (spec) where

import Test.Hspec
import Business.Add (addDependency)
import Core.Types
import qualified Data.Text.IO as TIO
import System.Directory (removeFile, doesFileExist)

spec :: Spec
spec = describe "Business.Add Debug" $ do
  it "debugs: adds a dependency with explicit version" $ do
    let path = "debug_add.cabal"
        content = "cabal-version: 2.4\nname: debug-add\nversion: 0.1.0.0\n\nlibrary\n    build-depends: base"
    TIO.writeFile path content
    let opts = AddOptions 
          { aoVersion = Just ">=1.0"
          , aoSection = TargetLib
          , aoCondition = Nothing
          , aoFlag = Nothing
          , aoDev = False
          , aoDryRun = False
          , aoCheck = False
          , aoInteractive = False
          , aoStrategy = StrategyCaret
          , aoGit = Nothing
          , aoTag = Nothing
          , aoPath = Nothing, aoMixin = Nothing
          , aoPackageNames = ["text"]
          }
    result <- addDependency Nothing opts path
    case result of
      Right () -> do
          newContent <- TIO.readFile path
          putStrLn "\n--- GENERATED CONTENT ---"
          TIO.putStrLn newContent
          putStrLn "--- END ---"
      Left err -> putStrLn $ "Error: " ++ show err
    
    exists <- doesFileExist path
    if exists then removeFile path else return ()
    True `shouldBe` True

  it "debugs: adds to multi-line" $ do
    let path = "debug_add_multi.cabal"
        content = "library\n    build-depends:\n        base,"
    TIO.writeFile path content
    let opts = AddOptions 
          { aoVersion = Just ">=2.0"
          , aoSection = TargetLib
          , aoCondition = Nothing
          , aoFlag = Nothing
          , aoDev = False
          , aoDryRun = False
          , aoCheck = False
          , aoInteractive = False
          , aoStrategy = StrategyCaret
          , aoGit = Nothing
          , aoTag = Nothing
          , aoPath = Nothing, aoMixin = Nothing
          , aoPackageNames = ["aeson"]
          }
    result <- addDependency Nothing opts path
    case result of
      Right () -> do
          newContent <- TIO.readFile path
          putStrLn "\n--- [Multi] GENERATED CONTENT ---"
          TIO.putStrLn newContent
          putStrLn "--- END ---"
      Left err -> putStrLn $ "Error: " ++ show err
    
    exists <- doesFileExist path
    if exists then removeFile path else return ()
    True `shouldBe` True

  it "debugs: adds to test-suite" $ do
    let path = "debug_add_test.cabal"
        content = "cabal-version: 2.4\nname: debug-add\nversion: 0.1.0.0\n\ntest-suite my-test\n    type: exitcode-stdio-1.0\n    build-depends: base"
    TIO.writeFile path content
    let opts = AddOptions 
          { aoVersion = Nothing
          , aoSection = TargetTest (Just "my-test")
          , aoCondition = Nothing
          , aoFlag = Nothing
          , aoDev = False
          , aoDryRun = False
          , aoCheck = False
          , aoInteractive = False
          , aoStrategy = StrategyCaret
          , aoGit = Nothing
          , aoTag = Nothing
          , aoPath = Nothing, aoMixin = Nothing
          , aoPackageNames = ["hspec"]
          }
    result <- addDependency Nothing opts path
    case result of
      Right () -> do
          newContent <- TIO.readFile path
          putStrLn "\n--- [Test-Suite] GENERATED CONTENT ---"
          TIO.putStrLn newContent
          putStrLn "--- END ---"
      Left err -> putStrLn $ "Error: " ++ show err
    
    exists <- doesFileExist path
    if exists then removeFile path else return ()
    True `shouldBe` True