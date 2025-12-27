{-# LANGUAGE OverloadedStrings #-}

module Business.SubLibrarySpec (spec) where

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
spec = describe "Sub-library Support" $ do
  
  it "adds a dependency to a named sub-library" $ do
    withTempCabalFile cabalWithSubLib $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["containers"]
            , aoVersion = Just ">=0.6"
            , aoSection = TargetNamed "sublib"
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
      -- Verify it's in the sublib section
      let (mainLib, subLib) = T.breakOn "library sublib" content
      T.unpack subLib `shouldContain` "containers >=0.6"
      T.unpack mainLib `shouldNotContain` "containers >=0.6"

  it "targets main library when TargetLib is used even if sub-libraries exist" $ do
    withTempCabalFile cabalWithSubLib $ \path -> do
      let opts = AddOptions 
            { aoPackageNames = ["mtl"]
            , aoVersion = Just ">=2.2"
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
      let (mainLib, subLib) = T.breakOn "library sublib" content
      T.unpack mainLib `shouldContain` "mtl >=2.2"
      T.unpack subLib `shouldNotContain` "mtl >=2.2"

cabalWithSubLib :: Text
cabalWithSubLib = T.unlines
  [ "cabal-version:      3.0"
  , "name:               sublib-project"
  , "version:            0.1.0.0"
  , ""
  , "library"
  , "    exposed-modules:  MainLib"
  , "    build-depends:    base >=4.14"
  , "    default-language: Haskell2010"
  , ""
  , "library sublib"
  , "    exposed-modules:  SubLib"
  , "    build-depends:    base"
  , "    default-language: Haskell2010"
  ]

-- Helpers

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "temp_sublib_spec.cabal"
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
