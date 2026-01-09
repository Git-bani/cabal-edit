{-# LANGUAGE OverloadedStrings #-}

module Core.SolverSpec (spec) where

import Test.Hspec
import Core.Solver (verifyChanges)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)


spec :: Spec
spec = describe "Core.Solver" $ do
  
  it "restores original file when solver fails" $ do
    let originalContent = "name: test-pkg\nversion: 0.1.0.0\ncabal-version: 3.0\nlibrary\n"
    let badContent = "This is not a valid cabal file content"
    
    withTempCabalFile originalContent $ \path -> do
      -- The solver check should fail because \"badContent\" is invalid syntax
      -- or because it can't find dependencies (since it's garbage)
      -- or simply because 'cabal build' returns non-zero.
      _ <- verifyChanges path badContent
      
      -- It might fail or succeed depending on how robust 'cabal build --dry-run' is 
      -- against total garbage, but usually it fails.
      -- If it fails, we check restoration. 
      
      -- Even if verifyChanges returns Left (failure), the file MUST be restored.
      finalContent <- TIO.readFile path
      finalContent `shouldBe` originalContent

  it "restores original file even if exception occurs (simulated)" $ do
     -- This is harder to test without injecting faults into verifyChanges.
     -- We rely on the first test to cover the general "failure restores file" path.
     pendingWith "Requires dependency injection to simulate process exceptions"

withTempCabalFile :: T.Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_solver_test.cabal"
  -- verifyChanges uses path <> ".bak"
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
