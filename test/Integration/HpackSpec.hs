{-# LANGUAGE OverloadedStrings #-}

module Integration.HpackSpec (spec) where

import Test.Hspec
import System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode, readProcess)
import Control.Exception (bracket, catch, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

spec :: Spec
spec = do
  exePath <- runIO $ do
    out <- readProcess "cabal" ["list-bin", "exe:cabal-edit"] ""
    return $ trim out

  describe "Hpack Detection" $ do
    
    it "warns when package.yaml is present" $ do
      withTempHpackProject $ \_dir cabalPath -> do
        -- Use absolute path to exe and relative path to .cabal
        (_code, _stdout, stderr) <- readProcessWithExitCode exePath ["--verbose", "add", "aeson", "--dry-run"] ""
        
        -- Should succeed but log warning to stderr
        let err = T.pack stderr
        err `shouldSatisfy` T.isInfixOf "[WARNING] package.yaml detected"
        err `shouldSatisfy` T.isInfixOf "overwritten by hpack"

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

withTempHpackProject :: (FilePath -> FilePath -> IO a) -> IO a
withTempHpackProject action = do
  withTempDir "hpack_project" $ \dir -> do
    let cabalFile = dir </> "test.cabal"
    let hpackFile = dir </> "package.yaml"
    TIO.writeFile cabalFile "name: test\nversion: 0.1\nlibrary\n  build-depends: base"
    TIO.writeFile hpackFile "name: test\ndependencies:\n  - base"
    setCurrentDirectory dir
    action dir "test.cabal"

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