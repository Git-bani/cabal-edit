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
import Control.Monad (when, forM_)
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text as T
import Core.AST.Parser (parseAST)
import Core.AST.Editor (findDependenciesInAST)
import Data.List (sort)

spec :: Spec
spec = describe "Golden Roundtrip" $ do
  
  let fixtures = ["complex.cabal", "common-stanzas.cabal", "lens.cabal", "aeson.cabal", "pandoc.cabal"]
  
  forM_ fixtures $ \fixture -> do
    it ("preserves " ++ fixture ++ " exactly after Add + Remove cycle") $ do
      withFixture fixture $ \path -> do
        original <- TIO.readFile path
        
        -- 1. Add a unique dependency
        let addOpts = AddOptions 
              { aoPackageNames = ["cabal-edit-unique-dep"]
              , aoVersion = Just ">=0.1"
              , aoSection = TargetLib
              , aoCondition = Nothing, aoFlag = Nothing
              , aoDev = False
              , aoDryRun = False
              , aoGit = Nothing
              , aoTag = Nothing
              , aoPath = Nothing
              , aoInteractive = False
              }        
        resAdd <- addDependency Nothing addOpts path
        resAdd `shouldSatisfy` isSuccess
        
        -- 2. Remove the same dependency
        let rmOpts = RemoveOptions
              { roPackageNames = ["cabal-edit-unique-dep"]
              , roSection = TargetLib
              , roDryRun = False, roInteractive = False
              }
              
        resRm <- removeDependency rmOpts path
        resRm `shouldSatisfy` isSuccess
        
        -- 3. Verify content match
        final <- TIO.readFile path
        final `shouldBe` original

    it ("is idempotent when adding " ++ fixture) $ do
      withFixture fixture $ \path -> do
        let addOpts = AddOptions 
              { aoPackageNames = ["bytestring"]
              , aoVersion = Just ">=0.10"
              , aoSection = TargetLib
              , aoCondition = Nothing, aoFlag = Nothing
              , aoDev = False
              , aoDryRun = False
              , aoGit = Nothing
              , aoTag = Nothing
              , aoPath = Nothing
              , aoInteractive = False
              }
        -- First add
        _ <- addDependency Nothing addOpts path
        content1 <- TIO.readFile path
        
        -- Second add (same)
        _ <- addDependency Nothing addOpts path
        content2 <- TIO.readFile path
        
        content2 `shouldBe` content1

  it "Property: Add + Remove is semantic Identity on random names" $ hedgehog $ do
    -- Use a dummy cabal file
    let baseContent = "cabal-version: 3.0\nname: test\nversion: 0\nlibrary\n  build-depends: base\n"
    firstChar <- forAll Gen.alpha
    rest <- forAll $ Gen.text (Range.linear 1 14) Gen.alphaNum
    let pkgName = T.cons firstChar rest
    -- Skip 'base' to avoid confusion
    when (pkgName /= "base") $ do
      (originalDeps, finalDeps) <- evalIO $ do
        cwd <- getCurrentDirectory
        let path = cwd </> "prop_test.cabal"
        TIO.writeFile path baseContent
        
        -- Get original deps
        content0 <- TIO.readFile path
        let ast0 = parseAST content0
        let deps0 = sort $ map (\(_,_,d) -> unPackageName $ depName d) $ findDependenciesInAST ast0

        let addOpts = AddOptions { aoPackageNames = [pkgName], aoVersion = Just ">=0", aoSection = TargetLib, aoCondition = Nothing, aoFlag = Nothing, aoDev = False, aoDryRun = False, aoGit = Nothing, aoTag = Nothing, aoPath = Nothing, aoInteractive = False }
        _ <- addDependency Nothing addOpts path
        
        let rmOpts = RemoveOptions { roPackageNames = [pkgName], roSection = TargetLib, roDryRun = False, roInteractive = False }
        _ <- removeDependency rmOpts path
        
        -- Get final deps
        content1 <- TIO.readFile path
        let ast1 = parseAST content1
        let deps1 = sort $ map (\(_,_,d) -> unPackageName $ depName d) $ findDependenciesInAST ast1

        ignoringIOErrors $ removeFile path
        ignoringIOErrors $ removeFile (path ++ ".bak")
        return (deps0, deps1)
      
      finalDeps === originalDeps


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
