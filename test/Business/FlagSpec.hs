{-# LANGUAGE OverloadedStrings #-}

module Business.FlagSpec (spec) where
import Data.Either (isRight, isLeft)

import Test.Hspec
import Business.Flag
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Cabal Flag Management" $ do
  
  it "adds a new flag" $ do
    withTempCabalFile basicCabal $ \path -> do
      let opts = FlagOptions (Just "dev") FlagAdd False False
      result <- handleFlag opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "flag dev"
      T.unpack content `shouldContain` "manual: True"

  it "enables an existing flag" $ do
    let cabalWithFlag = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag dev", "    default: False"
          , "library", "    build-depends: base"
          ]
    withTempCabalFile cabalWithFlag $ \path -> do
      let opts = FlagOptions (Just "dev") FlagEnable False False
      result <- handleFlag opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "default: True"

  it "disables an existing flag" $ do
    let cabalWithFlag = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag dev", "    default: True"
          , "library", "    build-depends: base"
          ]
    withTempCabalFile cabalWithFlag $ \path -> do
      let opts = FlagOptions (Just "dev") FlagDisable False False
      result <- handleFlag opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "default: False"

  it "removes an existing flag" $ do
    let cabalWithFlag = T.unlines
          [ "cabal-version: 2.4", "name: test", "version: 0.1"
          , "flag dev", "    default: True"
          , "library", "    build-depends: base"
          ]
    withTempCabalFile cabalWithFlag $ \path -> do
      let opts = FlagOptions (Just "dev") FlagRemove False False
      result <- handleFlag opts path
      result `shouldSatisfy` isRight
      
      content <- TIO.readFile path
      T.unpack content `shouldNotContain` "flag dev"

basicCabal :: Text
basicCabal = T.unlines
  [ "cabal-version: 2.4"
  , "name: test-project"
  , "version: 0.1.0.0"
  , "library"
  , "    build-depends: base"
  ]

-- Helpers


withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_flag_spec.cabal"
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