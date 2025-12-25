{-# LANGUAGE OverloadedStrings #-}

module Core.SafetySpec (spec) where

import Core.Safety
import Core.Types
import Test.Hspec
import System.Directory (getCurrentDirectory, removeFile, doesFileExist, removeDirectory)
import System.FilePath ((</>), (<.>))
import Control.Exception (bracket, catch, IOException)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

spec :: Spec
spec = describe "Core.Safety" $ do
  
  describe "createBackup" $ do
    it "creates a .bak file" $ do
      withTempFile "test.cabal" "content" $ \path -> do
        createBackup path
        let bakPath = path <.> "bak"
        exists <- doesFileExist bakPath
        exists `shouldBe` True
        content <- TIO.readFile bakPath
        content `shouldBe` "content"

  describe "safeWriteCabal" $ do
    it "writes file when valid" $ do
      withTempFile "valid.cabal" "name: foo" $ \path -> do
        let newContent = "name: foo\nversion: 1.0"
        res <- safeWriteCabal path newContent
        res `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        content `shouldBe` newContent

    it "fails to write invalid cabal content" $ do
      withTempFile "invalid.cabal" "name: foo" $ \path -> do
        let newContent = "this is not a valid cabal file : -"
        res <- safeWriteCabal path newContent
        
        case res of
          Failure (Error _ ParseError) -> return ()
          _ -> expectationFailure $ "Should have failed with ParseError, got: " ++ show res
        
        -- File should retain original content
        content <- TIO.readFile path
        content `shouldBe` "name: foo"

  describe "verifyCabalContent" $ do
    it "validates good content" $ do
      let valid = "name: foo\nversion: 0.1\nlibrary\n  build-depends: base"
      verifyCabalContent valid `shouldSatisfy` isRight

    it "rejects bad content" $ do
      let invalid = "name: foo\n  bad-indent"
      verifyCabalContent invalid `shouldSatisfy` isLeft

-- Helpers

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

withTempFile :: FilePath -> Text -> (FilePath -> IO a) -> IO a
withTempFile name content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> name
  bracket 
    (TIO.writeFile path content >> return path)
    (\p -> do
        ignoringIOErrors $ removeFile p
        ignoringIOErrors $ removeFile (p <.> "bak")
        ignoringIOErrors $ removeDirectory (p <.> "lock") -- Clean up lock if left
    )
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ()) 
