#!/usr/bin/env cabal
{-# LANGUAGE OverloadedStrings #-}

module Integration.WorkspaceSpec (spec) where
import Data.Either (isRight, isLeft)

import Test.Hspec
import System.Directory
import System.FilePath
import System.Process (callProcess, readProcess)
import Control.Exception (bracket, catch, IOException)
import qualified Data.Text as T
-- import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

spec :: Spec
spec = do
  exePath <- runIO $ do
    out <- readProcess "cabal" ["list-bin", "exe:cabal-edit"] ""
    return $ trim out

  describe "Workspace Support" $ do
    
    it "adds a local workspace member as a dependency without constraint" $ do
      withTempWorkspace $ \dir -> do
        let run args = callProcess exePath args
        
        -- We have pkg-a and pkg-b in workspace.
        -- Add pkg-b to pkg-a.
        run ["-p", "pkg-a", "add", "pkg-b"]
        
        content <- TIO.readFile (dir </> "pkg-a" </> "pkg-a.cabal")
        -- Should have pkg-b without version constraint (WorkspaceVersion -> empty string)
        T.unpack content `shouldContain` "pkg-b"
        T.unpack content `shouldNotContain` "pkg-b ^>="

    it "targets a specific package in the workspace using --package" $ do
      withTempWorkspace $ \dir -> do
        let run args = callProcess exePath args
        
        -- Add 'aeson' only to pkg-b
        -- Global flags BEFORE command, local flags AFTER command
        run ["--package", "pkg-b", "add", "--version", ">=2.0", "aeson"]
        
        contentA <- TIO.readFile (dir </> "pkg-a" </> "pkg-a.cabal")
        contentB <- TIO.readFile (dir </> "pkg-b" </> "pkg-b.cabal")
        
        T.unpack contentB `shouldContain` "aeson >=2.0"
        T.unpack contentA `shouldNotContain` "aeson"

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Setup Helpers

withTempWorkspace :: (FilePath -> IO a) -> IO a
withTempWorkspace action = do
  withTempDir "ws_integration" $ \dir -> do
    TIO.writeFile (dir </> "cabal.project") "packages: pkg-a/ pkg-b/\n"
    
    createDirectoryIfMissing True (dir </> "pkg-a")
    TIO.writeFile (dir </> "pkg-a" </> "pkg-a.cabal") $ T.unlines
      [ "cabal-version: 2.4", "name: pkg-a", "version: 0.1", "library", "  build-depends: base" ]
      
    createDirectoryIfMissing True (dir </> "pkg-b")
    TIO.writeFile (dir </> "pkg-b" </> "pkg-b.cabal") $ T.unlines
      [ "cabal-version: 2.4", "name: pkg-b", "version: 0.1", "library", "  build-depends: base" ]
      
    setCurrentDirectory dir
    action dir

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
  cwd <- getCurrentDirectory
  let path = cwd </> ("temp_" <> name)
  
  bracket 
    (createDirectoryIfMissing True path >> return path)
    (\p -> do
        setCurrentDirectory cwd
        ignoringIOErrors $ removeDirectoryRecursive p
    ) 
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())