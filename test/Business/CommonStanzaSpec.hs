{-# LANGUAGE OverloadedStrings #-}

module Business.CommonStanzaSpec (spec) where
import Data.Either (isRight, isLeft)

import Test.Hspec
import Business.Add
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Common Stanza Support" $ do
  
  it "adds a dependency to a common stanza" $ do
    withTempCabalFile commonCabal $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["aeson"]
            , aoVersion = Nothing
            , aoSection = TargetNamed "shared-props" -- Matches 'common shared-props'
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
          , aoInteractive = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      let (_before, common) = T.breakOn "common shared-props" content
      T.unpack common `shouldContain` "aeson ^>="
      T.unpack _before `shouldNotContain` "aeson"

commonCabal :: Text
commonCabal = T.unlines
  [ "cabal-version:      3.0"
  , "name:               common-test"
  , "version:            0.1.0.0"
  , ""
  , "common shared-props"
  , "    default-language: Haskell2010"
  , "    build-depends:    base"
  , ""
  , "library"
  , "    import:           shared-props"
  , "    exposed-modules:  MyLib"
  ]

-- Helpers


withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_common_spec.cabal"
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
