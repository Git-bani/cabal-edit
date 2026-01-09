{-# LANGUAGE OverloadedStrings #-}
module Business.SyncSpec (spec) where

import Test.Hspec
import Business.Sync
import Core.Types
import Core.ProjectContext (ProjectContext(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)

spec :: Spec
spec = describe "Business.Sync" $ do
  it "aligns dependencies to the highest version in the workspace" $ do
    withTempWorkspace [pkgA, pkgB] $ \ctx -> do
      let opts = SyncOptions { soDryRun = False, soLatest = False }
      result <- syncWorkspace opts ctx
      result `shouldSatisfy` isRight
      
      -- Verify pkgA (was 1.2.4) is now 2.0.0
      case listToMaybe (pcPackages ctx) of
        Just (_, path) -> do
          contentA <- TIO.readFile path
          T.unpack contentA `shouldContain` "text ==2.0.0"
        Nothing -> expectationFailure "No packages in context"
      
      -- Verify pkgB (was already 2.0.0) is still 2.0.0
      let pkgBPath = snd $ pcPackages ctx !! 1
      contentB <- TIO.readFile pkgBPath
      T.unpack contentB `shouldContain` "text ==2.0.0"

  it "aligns all dependencies to latest from hackage when requested" $ do
    -- This test is slow/network dependent, but we can verify it calls the logic.
    pendingWith "Integration test with network"

pkgA :: (Text, Text)
pkgA = ("pkg-a", T.unlines
  [ "cabal-version: 2.4"
  , "name: pkg-a"
  , "version: 0.1"
  , "library"
  , "  build-depends: base, text ==1.2.4"
  ])

pkgB :: (Text, Text)
pkgB = ("pkg-b", T.unlines
  [ "cabal-version: 2.4"
  , "name: pkg-b"
  , "version: 0.1"
  , "library"
  , "  build-depends: base, text ==2.0.0"
  ])

withTempWorkspace :: [(Text, Text)] -> (ProjectContext -> IO a) -> IO a
withTempWorkspace pkgs action = do
  cwd <- getCurrentDirectory
  let tmpDir = cwd </> "temp_sync_test"
  bracket
    (do
      createDirectoryIfMissing True tmpDir
      paths <- mapM (writePkg tmpDir) pkgs
      let ctx = ProjectContext 
            { pcRoot = tmpDir
            , pcPackages = paths
            , pcPackageGlobs = []
            }
      return ctx
    )
    (\_ -> ignoringIOErrors $ removeDirectoryRecursive tmpDir)
    action
  where
    writePkg root (name, content) = do
      let pkgDir = root </> T.unpack name
      createDirectoryIfMissing True pkgDir
      let cabalPath = pkgDir </> T.unpack (name <> ".cabal")
      TIO.writeFile cabalPath content
      return (trustedMkPackageName name, cabalPath)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())
