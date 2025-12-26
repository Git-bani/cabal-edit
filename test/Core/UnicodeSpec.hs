{-# LANGUAGE OverloadedStrings #-}

module Core.UnicodeSpec (spec) where

import Test.Hspec
import Core.Types
import Business.Add
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Core.Unicode" $ do
  it "preserves Unicode characters when adding a dependency" $ do
    let unicodeContent = T.unlines
          [ "cabal-version: 3.0"
          , "name: unicode-test"
          , "version: 0.1.0.0"
          , "description: This package has unicode: λ value -> α + β"
          , "maintainer: Anitha Barns <anithabarns@gmail.com> -- © 2025"
          , ""
          , "library"
          , "  build-depends: base"
          , "  -- Comment with unicode: 你好"
          , "  default-language: Haskell2010"
          ]
    
    withTempCabalFile unicodeContent $ \path -> do
      let opts = AddOptions 
            { aoVersion = Nothing
            , aoSection = TargetLib
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            , aoPackageNames = ["text"]
            }
      result <- addDependency opts path
      
      case result of
        Failure e -> expectationFailure $ "Add failed: " ++ show e
        Success () -> do
          newContent <- TIO.readFile path
          let contentStr = T.unpack newContent
          
          -- Check Unicode preservation
          contentStr `shouldContain` "λ value -> α + β"
          contentStr `shouldContain` "© 2025"
          contentStr `shouldContain` "你好"
          
          -- Check dependency addition
          contentStr `shouldContain` "text"

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "unicode_test.cabal"
  bracket 
    (TIO.writeFile path content >> return path)
    (\p -> ignoringIOErrors $ removeFile p)
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())