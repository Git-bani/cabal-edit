{-# LANGUAGE OverloadedStrings #-}

module Business.RemoveSpec (spec) where

import Test.Hspec
import Business.Remove
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Business.Remove" $ do
  
  it "removes an existing dependency from library" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = RemoveOptions 
            { roPackageNames = ["text"]
            , roSection = TargetLib
            , roDryRun = False
            }
      
      result <- removeDependency opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldNotContain` "text"
      T.unpack content `shouldContain` "base >=4.14"

  it "fails if dependency does not exist" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = RemoveOptions 
            { roPackageNames = ["non-existent-pkg"]
            , roSection = TargetLib
            , roDryRun = False
            }
      
      result <- removeDependency opts path
      result `shouldSatisfy` isFailure
      -- verify content unchanged
      content <- TIO.readFile path
      T.unpack content `shouldContain` "text"

  it "removes from a specific section (test-suite)" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = RemoveOptions 
            { roPackageNames = ["hspec"]
            , roSection = TargetTest (Just "my-test")
            , roDryRun = False
            }
      
      result <- removeDependency opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldNotContain` "hspec"

  it "removes multiple packages in a single command" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = RemoveOptions 
            { roPackageNames = ["base", "text"]
            , roSection = TargetLib
            , roDryRun = False
            }
      
      result <- removeDependency opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldNotContain` "base"
      T.unpack content `shouldNotContain` "text"

  it "automatically detects and removes dependency from non-default section" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      -- 'hspec' is ONLY in my-test suite.
      -- If we use default section (TargetLib), it should find it in my-test.
      let opts = RemoveOptions 
            { roPackageNames = ["hspec"]
            , roSection = TargetLib
            , roDryRun = False
            }
      
      result <- removeDependency opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldNotContain` "hspec"

  describe "Business.Remove (Outliers)" $ do
    it "removes dependency from conditional block" $ do
      let cabalWithIf = T.unlines
            [ "cabal-version: 2.4", "name: if-test", "version: 0.1", "library"
            , "  if os(windows)"
            , "    build-depends: Win32, base"
            ]
      withTempCabalFile cabalWithIf $ \path -> do
        let opts = RemoveOptions 
              { roPackageNames = ["Win32"]
              , roSection = TargetLib
              , roDryRun = False
              }
        result <- removeDependency opts path
        result `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        T.unpack content `shouldNotContain` "Win32"
        T.unpack content `shouldContain` "base"

    it "handles removal when it is the last dependency" $ do
       let lastDepCabal = T.unlines
            [ "cabal-version: 2.4", "name: last", "version: 0.1", "library"
            , "  build-depends: base"
            ]
       withTempCabalFile lastDepCabal $ \path -> do
        let opts = RemoveOptions 
              { roPackageNames = ["base"]
              , roSection = TargetLib
              , roDryRun = False
              }
        result <- removeDependency opts path
        result `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        -- Should have removed base
        T.unpack content `shouldNotContain` "base"
        -- Implementation detail: does it leave empty build-depends? 
        -- Core.Serializer.removeDependencyLine should handle it.
        -- Let's just check 'base' is gone.

basicCabalFile :: Text
basicCabalFile = T.unlines
  [ "cabal-version:      2.4"
  , "name:               test-project"
  , "version:            0.1.0.0"
  , ""
  , "library"
  , "    exposed-modules:  MyLib"
  , "    build-depends:    base >=4.14"
  , "                    , text"
  , "    default-language: Haskell2010"
  , ""
  , "test-suite my-test"
  , "    type:             exitcode-stdio-1.0"
  , "    main-is:          Spec.hs"
  , "    build-depends:    base"
  , "                    , hspec"
  , "    default-language: Haskell2010"
  ]

-- Helpers

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

isFailure :: Result a -> Bool
isFailure (Failure _) = True
isFailure _ = False

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_remove_spec.cabal"
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