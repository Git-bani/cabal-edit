{-# LANGUAGE OverloadedStrings #-}

module Business.SourceDepSpec (spec) where
import Data.Either (isRight)

import Test.Hspec
import Business.Add
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile, setCurrentDirectory, createDirectoryIfMissing, removeDirectoryRecursive, getTemporaryDirectory)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Business.SourceDep" $ do
  
  it "adds a git dependency to cabal.project and .cabal" $ do
    withTempEnv basicCabalFile $ \_dir cabalPath projPath -> do
      let opts = AddOptions 
            { aoPackageNames = ["foo"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
          , aoInteractive = False
            , aoGit = Just "https://github.com/example/foo"
            , aoTag = Just "v1.0"
            , aoPath = Nothing, aoMixin = Nothing
            }
      
      -- Ensure we are in the temp dir context effectively (or passed paths are absolute)
      -- addDependency takes a file path to .cabal. It will search for cabal.project relative to it.
      -- Since we are passing absolute paths (from withTempEnv), logic in ensureProjectFile might rely on CWD or relative search.
      -- core logic `findProjectRoot` looks up from CWD.
      -- So we must set CWD or ensure logic traverses up from cabal file.
      -- `findProjectRoot` uses `getCurrentDirectory`. So we must change dir.
      
      result <- addDependency Nothing opts cabalPath
      result `shouldSatisfy` isRight
      
      -- Check .cabal file
      cabalContent <- TIO.readFile cabalPath
      T.unpack cabalContent `shouldContain` "foo" 
      -- Should have no version constraint (AnyVersion) by default for source deps
      -- AnyVersion formats to empty string in formatVersionConstraint.
      -- So line should be "build-depends: ... foo" or "foo,"
      T.unpack cabalContent `shouldContain` "foo"
      
      -- Check cabal.project
      projContent <- TIO.readFile projPath
      T.unpack projContent `shouldContain` "source-repository-package"
      T.unpack projContent `shouldContain` "type: git"
      T.unpack projContent `shouldContain` "location: https://github.com/example/foo"
      T.unpack projContent `shouldContain` "tag: v1.0"

  it "adds a local path dependency to cabal.project and .cabal" $ do
    withTempEnv basicCabalFile $ \_dir cabalPath projPath -> do
      let opts = AddOptions 
            { aoPackageNames = ["bar"]
            , aoVersion = Nothing
            , aoSection = TargetLib
            , aoCondition = Nothing, aoFlag = Nothing
            , aoDev = False
            , aoDryRun = False
          , aoInteractive = False
            , aoGit = Nothing
            , aoTag = Nothing
            , aoPath = Just "../bar"
            , aoMixin = Nothing
            }
      
      result <- addDependency Nothing opts cabalPath
      result `shouldSatisfy` isRight
      
      -- Check cabal.project
      projContent <- TIO.readFile projPath
      T.unpack projContent `shouldContain` "packages: ../bar"

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

-- Helpers


withTempEnv :: Text -> (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withTempEnv content action = do
  sysTemp <- getTemporaryDirectory
  let tempDir = sysTemp </> "temp_source_dep_test"
  let cabalPath = tempDir </> "test.cabal"
  let projPath = tempDir </> "cabal.project"
  
  bracket 
    (do
      safeCreateDirectory tempDir
      TIO.writeFile cabalPath content
      -- We don't create cabal.project, we expect it to be created OR we create it empty
      -- Logic says ensureProjectFile creates it if missing.
      return tempDir
    )
    (\_ -> ignoringIOErrors $ do 
      removeFile cabalPath
      removeFile projPath
      safeRemoveDirectory tempDir
    ) 
    (\_ -> do
        -- Change CWD to tempDir so findProjectRoot works
        bracket 
          (getCurrentDirectory <* setCurrentDirectory tempDir)
          (\old -> setCurrentDirectory old)
          (\_ -> action tempDir cabalPath projPath)
    )

safeCreateDirectory :: FilePath -> IO ()
safeCreateDirectory dir = do
  createDirectoryIfMissing True dir

safeRemoveDirectory :: FilePath -> IO ()
safeRemoveDirectory dir = do
  removeDirectoryRecursive dir

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())
