{-# LANGUAGE OverloadedStrings #-}

module Core.ProjectContextSpec (spec) where

import Test.Hspec
import Core.ProjectContext
import System.Directory
import System.FilePath
import qualified Data.Text.IO as TIO
import Control.Exception (bracket, catch, IOException)
import Data.List (isSuffixOf)

spec :: Spec
spec = describe "Core.ProjectContext" $ do
  
  describe "findProjectRoot" $ do
    it "finds root when cabal.project exists" $ do
      withTempWorkspace $ \root -> do
        found <- findProjectRoot
        found `shouldBe` Just root

    it "returns Nothing when no cabal.project exists" $ do
      withTempDir $ \dir -> do
        setCurrentDirectory dir
        found <- findProjectRoot
        found `shouldBe` Nothing

  describe "loadProjectContext" $ do
    it "parses packages and globs correctly" $ do
      withTempWorkspace $ \root -> do
        TIO.writeFile (root </> "cabal.project") $ 
          "packages: pkg_a \n" <> 
          "          libs/*\n" <> 
          "-- packages: ignored\n" <> 
          "optional-packages: pkg_opt"
        
        ctx <- loadProjectContext root
        pcRoot ctx `shouldBe` root
        pcPackageGlobs ctx `shouldContain` ["pkg_a"]
        pcPackageGlobs ctx `shouldContain` ["libs/*"]
        pcPackageGlobs ctx `shouldContain` ["pkg_opt"]
        pcPackageGlobs ctx `shouldNotContain` ["ignored"]

  describe "findAllPackageFiles" $ do
    it "finds all relevant .cabal files" $ do
      withTempWorkspace $ \root -> do
        createDirectoryIfMissing True (root </> "pkg_a")
        createDirectoryIfMissing True (root </> "libs" </> "pkg_b")
        createDirectoryIfMissing True (root </> "pkg_opt")
        
        writeFile (root </> "pkg_a" </> "pkg_a.cabal") "name: pkg_a"
        writeFile (root </> "libs" </> "pkg_b" </> "pkg_b.cabal") "name: pkg_b"
        writeFile (root </> "pkg_opt" </> "pkg_opt.cabal") "name: pkg_opt"
        
        TIO.writeFile (root </> "cabal.project") $ 
          "packages: pkg_a libs/*\n" <> 
          "optional-packages: pkg_opt"
        
        ctx <- loadProjectContext root
        files <- findAllPackageFiles ctx
        
        length files `shouldSatisfy` (>= 3)
        files `shouldSatisfy` any (\f -> "pkg_a.cabal" `isSuffixOf` f)
        files `shouldSatisfy` any (\f -> "pkg_b.cabal" `isSuffixOf` f)
        files `shouldSatisfy` any (\f -> "pkg_opt.cabal" `isSuffixOf` f)

-- Helpers

withTempWorkspace :: (FilePath -> IO a) -> IO a
withTempWorkspace action = withTempDir $ \dir -> do
  TIO.writeFile (dir </> "cabal.project") "packages: ."
  setCurrentDirectory dir
  action dir

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "spec_temp_workspace"
  
  bracket 
    (createDirectoryIfMissing True path >> return path)
    (\p -> do
        setCurrentDirectory cwd
        ignoringIOErrors $ removeDirectoryRecursive p
    ) 
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())