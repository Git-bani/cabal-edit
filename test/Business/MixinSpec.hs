{-# LANGUAGE OverloadedStrings #-}

module Business.MixinSpec (spec) where

import Test.Hspec
import Business.Add
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)
import Data.Either (isRight)

spec :: Spec
spec = describe "Business.Mixin" $ do
  
  it "adds a mixin with renaming" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["text"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoCheck = False
            , aoInteractive = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            , aoMixin = Just "(Data.Text as Text)"
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "mixins:"
      T.unpack content `shouldContain` "text (Data.Text as Text)"

  it "adds a mixin with hiding" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["base"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoCheck = False
            , aoInteractive = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            , aoMixin = Just "hiding (Prelude)"
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "mixins:"
      T.unpack content `shouldContain` "base hiding (Prelude)"

basicCabalFile :: Text
basicCabalFile = T.unlines
  [ "cabal-version:      2.4"
  , "name:               test-project"
  , "version:            0.1.0.0"
  , ""
  , "library"
  , "    exposed-modules:  MyLib"
  , "    build-depends:    base >=4.14"
  , "    default-language: Haskell2010"
  ]

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_mixin_spec.cabal"
  bracket 
    (TIO.writeFile path content >> return path)
    (\p -> ignoringIOErrors $ removeFile p)
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())
