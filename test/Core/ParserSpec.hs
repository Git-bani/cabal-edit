{-# LANGUAGE OverloadedStrings #-}

module Core.ParserSpec (spec) where

import Test.Hspec
import Core.Parser
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, IOException)

spec :: Spec
spec = describe "Core.Parser" $ do
  
  describe "parseCabalFile" $ do
    it "parses a valid simple cabal file" $ do
      withTempCabalFile simpleCabal $ \path -> do
        result <- parseCabalFile path
        case result of
          Success cf -> do
            cfPackageName cf `shouldBe` unsafeMkPackageName "test-package"
            length (cfSections cf) `shouldSatisfy` (>= 1)
          Failure e -> expectationFailure $ "Parse failed: " ++ show e

    it "fails on malformed file" $ do
      withTempCabalFile "invalid content" $ \path -> do
        result <- parseCabalFile path
        case result of
          Failure (Error _ ParseError) -> return ()
          _ -> expectationFailure "Should have failed with ParseError"

  describe "findSectionPosition" $ do
    it "finds correct bounds for a library section" $ do
      let content = T.unlines 
            [ "name: foo"
            , ""
            , "library"
            , "  build-depends: base"
            , "  default-language: Haskell2010"
            , ""
            , "executable main"
            ]
      let TextSpan (TextOffset start) (TextOffset end) = findSectionPosition "library" content
      
      -- Let's check extracted content
      let extracted = T.take (end - start) (T.drop start content)
      extracted `shouldSatisfy` ("build-depends" `T.isInfixOf`)
      extracted `shouldSatisfy` ("default-language" `T.isInfixOf`)
      extracted `shouldNotSatisfy` ("executable" `T.isInfixOf`)

    it "ignores section headers inside block comments" $ do
      let content = T.unlines
            [ "name: foo"
            , ""
            , "{-"
            , "library"
            , "  build-depends: wrong-lib"
            , "-}"
            , ""
            , "library"
            , "  build-depends: correct-lib"
            ]
      let TextSpan (TextOffset start) (TextOffset end) = findSectionPosition "library" content
      let extracted = T.take (end - start) (T.drop start content)
      
      -- It should match the second library
      extracted `shouldSatisfy` ("correct-lib" `T.isInfixOf`)
      extracted `shouldNotSatisfy` ("wrong-lib" `T.isInfixOf`)

simpleCabal :: Text
simpleCabal = T.unlines
  ["cabal-version: 2.4"
  ,"name: test-package"
  ,"version: 0.1.0.0"
  ,""
  ,"library"
  ,"  build-depends: base"
  ,"  default-language: Haskell2010"
  ]

withTempCabalFile :: Text -> (FilePath -> IO a) -> IO a
withTempCabalFile content action = do
  cwd <- getCurrentDirectory
  let path = cwd </> "parser_spec_temp.cabal"
  bracket 
    (TIO.writeFile path content >> return path)
    (\p -> ignoringIOErrors $ removeFile p)
    action

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors act = catch act (\e -> let _ = (e :: IOException) in return ())