{-# LANGUAGE OverloadedStrings #-}

module Business.AddSpec (spec) where

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
spec = describe "Business.Add" $ do
  
  it "adds a dependency with explicit version to a library" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["aeson"]
            , aoVersion = Just "==2.0.0.0"
            , aoSection = TargetLib -- defaults to library
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "aeson ==2.0.0.0"
      -- Check formatting: comma should be leading if configured, but let's just check valid cabal syntax roughly
      -- The serializer ensures "build-depends: ... , aeson ==2.0.0.0" or similar
      T.unpack content `shouldContain` "text"

  it "preserves existing dependencies" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["mtl"]
            , aoVersion = Just "==2.2.2"
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      _ <- addDependency Nothing opts path
      content <- TIO.readFile path
      
      T.unpack content `shouldContain` "base >=4.14"
      T.unpack content `shouldContain` "mtl ==2.2.2"

  it "adds to a specific section (test-suite)" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["hspec"]
            , aoVersion = Just ">=2.8"
            , aoSection = TargetNamed "my-test"
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = True
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      -- Verify it's in the test section (simple containment check)
      T.unpack content `shouldContain` "hspec >=2.8"
  
  it "fails gracefully if section not found" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["hspec"]
            , aoVersion = Just ">=2.8"
            , aoSection = TargetNamed "non-existent-section"
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = True
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isFailure

  it "updates existing dependency version instead of duplicating" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["text"]
            , aoVersion = Just "==1.2.4.1"
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      -- Should have updated version
      T.unpack content `shouldContain` "text ==1.2.4.1"
      -- Should NOT have duplicate 'text' (appears once in original, should appear once in result)
      T.count "text" content `shouldBe` 1

  it "uses ^>= constraint by default when no version is specified" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["aeson"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      -- Check for ^>= and some version number
      content `shouldSatisfy` T.isInfixOf "aeson ^>="

  it "adds multiple packages in a single command" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["bytestring", "vector"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      
      result <- addDependency Nothing opts path
      result `shouldSatisfy` isSuccess
      
      content <- TIO.readFile path
      T.unpack content `shouldContain` "bytestring ^>="
      T.unpack content `shouldContain` "vector ^>="

  it "adds a dependency to an existing if block" $ do
    let cabalWithIf = T.unlines
          [ "cabal-version: 2.4", "name: if-test", "version: 0.1", "library"
          , "  if os(windows)", "    build-depends: Win32"
          ]
    withTempCabalFile cabalWithIf $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["directory"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Just "os(windows)", aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      _ <- addDependency Nothing opts path
      content <- TIO.readFile path
      T.unpack content `shouldContain` "Win32"
      T.unpack content `shouldContain` "directory"

  it "creates a new if block if it doesn't exist" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["unix"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Just "os(linux)", aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      _ <- addDependency Nothing opts path
      content <- TIO.readFile path
      T.unpack content `shouldContain` "if os(linux)"
      T.unpack content `shouldContain` "unix"

  it "integrates --if flag through CLI options" $ do
    withTempCabalFile basicCabalFile $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["Win32"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Just "os(windows)", aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Nothing
            }
      _ <- addDependency Nothing opts path
      content <- TIO.readFile path
      T.unpack content `shouldContain` "if os(windows)"
      T.unpack content `shouldContain` "Win32"

  describe "Business.Add (Outliers)" $ do
    it "handles weird indentation gracefully" $ do
      let weirdCabal = T.unlines
            [ "cabal-version: 2.4", "name: weird", "version: 0.1", "library"
            , "  exposed-modules: Lib"
            , "  build-depends:"
            , "   base" -- 3 spaces
            , "   , text"
            ]
      withTempCabalFile weirdCabal $ \path -> do
        let opts = AddOptions 
              { aoPackageNames = ["aeson"]
              , aoVersion = Nothing
              , aoSection = TargetLib
              , aoCondition = Nothing, aoFlag = Nothing
              , aoDev = False
              , aoDryRun = False
              , aoGit = Nothing
              , aoTag = Nothing
              , aoPath = Nothing
              }
        result <- addDependency Nothing opts path
        result `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        -- Should insert aligned with existing
        T.unpack content `shouldContain` "aeson"

    it "does not confuse 'text' with 'text-conversions'" $ do
      let partialMatchCabal = T.unlines
            [ "cabal-version: 2.4", "name: partial", "version: 0.1", "library"
            , "  build-depends: base, text-conversions"
            ]
      withTempCabalFile partialMatchCabal $ \path -> do
        let opts = AddOptions 
              { aoPackageNames = ["text"]
              , aoVersion = Just "==2.0"
              , aoSection = TargetLib
              , aoCondition = Nothing, aoFlag = Nothing
              , aoDev = False
              , aoDryRun = False
              , aoGit = Nothing
              , aoTag = Nothing
              , aoPath = Nothing
              }
        result <- addDependency Nothing opts path
        result `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        -- Should add 'text' separately
        T.unpack content `shouldContain` "text ==2.0"
        T.unpack content `shouldContain` "text-conversions"
        -- Ensure we didn't accidentally edit text-conversions
        T.unpack content `shouldNotContain` "text-conversions ==2.0"

    it "supports adding with --flag option" $ do
      let cabalWithFlag = T.unlines
            [ "cabal-version: 2.4", "name: flag-test", "version: 0.1", "library"
            , "  exposed-modules: Lib"
            , "  build-depends: base"
            , ""
            , "flag enable-lens"
            , "  description: Enable lens support"
            , "  manual: True"
            , "  default: False"
            ]
      withTempCabalFile cabalWithFlag $ \path -> do
        let opts = AddOptions 
              { aoPackageNames = ["lens"]
              , aoVersion = Nothing
              , aoSection = TargetLib
              , aoCondition = Nothing, aoFlag = Just "enable-lens"
              , aoDev = False
              , aoDryRun = False
              , aoGit = Nothing
              , aoTag = Nothing
              , aoPath = Nothing
              }
        result <- addDependency Nothing opts path
        result `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        T.unpack content `shouldContain` "if flag(enable-lens)"
        T.unpack content `shouldContain` "lens"

    it "prioritizes explicit condition over flag" $ do
      withTempCabalFile basicCabalFile $ \path -> do
        let opts = AddOptions 
              { aoPackageNames = ["lens"]
              , aoVersion = Nothing
              , aoSection = TargetLib
              , aoCondition = Just "os(linux)", aoFlag = Just "ignored-flag"
              , aoDev = False
              , aoDryRun = False
              , aoGit = Nothing
              , aoTag = Nothing
              , aoPath = Nothing
              }
        result <- addDependency Nothing opts path
        result `shouldSatisfy` isSuccess
        
        content <- TIO.readFile path
        T.unpack content `shouldContain` "if os(linux)"
        T.unpack content `shouldNotContain` "flag(ignored-flag)"

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
  let path = cwd </> "temp_add_spec.cabal"
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