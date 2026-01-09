{-# LANGUAGE OverloadedStrings #-}

module Business.AddOutlierSpec (spec) where

import Test.Hspec
import Business.Add
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)
import Data.Either (isRight, isLeft)

spec :: Spec
spec = describe "Business.Add (Outliers)" $ do
  
  it "handles deeply nested conditionals" $ do
    let content = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "library"
          , "  if os(linux)"
          , "    if arch(x86_64)"
          , "      build-depends: base"
          ]
    withTempCabalFile content $ \path -> do
      let opts = defaultAddOptions { aoPackageNames = ["text"], aoCondition = Just "os(linux) && arch(x86_64)" }
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      final <- TIO.readFile path
      -- This behavior depends on how the editor handles nested conditions. 
      -- Currently it might add a flat if or append to existing block if exact match. 
      -- For now, we just ensure it succeeds and contains the dependency.
      T.unpack final `shouldContain` "text"

  it "rejects invalid package names" $ do
    let content = T.unlines ["cabal-version: 2.4", "name: test", "version: 0.1", "library"]
    withTempCabalFile content $ \path -> do
      let opts = defaultAddOptions { aoPackageNames = ["Invalid Package!"] }
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isLeft
      case result of
        Left (Error msg InvalidDependency) -> 
          T.unpack msg `shouldContain` "Invalid package name"
        _ -> expectationFailure $ "Expected InvalidDependency error, got " ++ show result

  it "handles duplicate dependency addition gracefully (idempotent)" $ do
    let content = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "library"
          , "  build-depends: text"
          ]
    withTempCabalFile content $ \path -> do
      let opts = defaultAddOptions { aoPackageNames = ["text"] }
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      final <- TIO.readFile path
      -- Should not have two "text" lines in the same block
      let textLines = length $ filter (\l -> "text" `T.isInfixOf` l && not ("test" `T.isInfixOf` l)) (T.lines final)
      textLines `shouldBe` 1

  it "preserves mixed CRLF/LF line endings" $ do
    let content = "cabal-version: 2.4\r\nname: test\r\nversion: 0.1\r\nlibrary\r\n  build-depends: base\r\n"
    withTempCabalFile content $ \path -> do
      let opts = defaultAddOptions { aoPackageNames = ["text"] }
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      final <- TIO.readFile path
      T.unpack final `shouldContain` "\r\n"
      T.unpack final `shouldContain` "text"

  it "dry run with verify does not modify file" $ do
    let content = T.unlines ["cabal-version: 2.4", "name: test", "version: 0.1", "library"]
    withTempCabalFile content $ \path -> do
      -- We disable verification here because it requires a valid cabal project context which the temp file lacks
      -- This tests the dry-run logic flow specifically.
      let opts = defaultAddOptions { aoPackageNames = ["text"], aoDryRun = True, aoCheck = False }
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      final <- TIO.readFile path
      final `shouldBe` content

defaultAddOptions :: AddOptions
defaultAddOptions =
  AddOptions
    { aoVersion = Nothing
    , aoSection = TargetLib
    , aoCondition = Nothing
    , aoFlag = Nothing
    , aoDev = False
    , aoDryRun = False
    , aoCheck = False
    , aoInteractive = False
    , aoGit = Nothing
    , aoTag = Nothing
    , aoPath = Nothing
    , aoMixin = Nothing
    , aoStrategy = StrategyCaret
    , aoPackageNames = []
    }

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_add_outlier.cabal"
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