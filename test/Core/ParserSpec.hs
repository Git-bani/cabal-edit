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
-- import Data.List (find)

import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Core.Types

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

    it "Property: parses any valid generated Cabal file" $ hedgehog $ do
      cabalContent <- forAll genValidCabalFile
      -- Use a safe temporary file wrapper for Hedgehog (or just manually write/delete in IO)
      -- Hedgehog's `evalIO` lets us run IO.
      
      -- We can't easily use withTempCabalFile inside hedgehog property as it expects IO, not PropertyT
      -- But we can just verify the content structure if we extracted parser logic to pure functions.
      -- However, parseCabalFile is IO based (uses Cabal lib).
      
      -- Let's assume we can write/read fast enough or use a unique name
      -- Actually, `parseCabalFile` takes a FilePath.
      -- Let's extract `parseCabalFile` logic or just verify we can parse it.
      
      -- For now, let's just assert that our generator produces something that looks like a cabal file
      -- and rely on the fact that if we passed it to Cabal, it should work.
      -- A true roundtrip test requires `parseCabalFile` to be exposed as pure or us to use IO.
      
      -- Let's use evalIO to write and parse.
      result <- evalIO $ do
        cwd <- getCurrentDirectory
        let path = cwd </> "gen_prop_test.cabal"
        TIO.writeFile path cabalContent
        res <- parseCabalFile path
        removeFile path
        return res
        
      case result of
        Success cf -> do
          -- Basic integrity checks
          assert $ not (T.null (cfRawContent cf))
          assert $ length (cfSections cf) >= 1
        Failure e -> do
           annotateShow cabalContent
           annotateShow e
           failure

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

-- Generators

genValidCabalFile :: Gen Text
genValidCabalFile = do
  name <- genPackageName
  version <- genVersion
  
  let header = T.unlines
        [ "cabal-version: 2.4"
        , "name: " <> unPackageName name
        , "version: " <> showVer version
        , ""
        ]
        
  libSection <- genLibrarySection
  
  return $ header <> libSection

genLibrarySection :: Gen Text
genLibrarySection = do
  deps <- genDependencies
  return $ T.unlines
    [ "library"
    , "  default-language: Haskell2010"
    , "  build-depends:"
    , "      " <> T.intercalate "\n    , " (map formatDep deps)
    ]

formatDep :: Dependency -> Text
formatDep dep = 
  let name = unPackageName (depName dep)
      constraint = maybe "" showConstraint (depVersionConstraint dep)
  in if T.null constraint 
     then name 
     else name <> " " <> constraint

showConstraint :: VersionConstraint -> Text
showConstraint AnyVersion = ""
showConstraint (ExactVersion v) = "==" <> showVer v
showConstraint (MajorBoundVersion v) = "^>=" <> showVer v
showConstraint (UnparsedVersion t) = t
showConstraint _ = ""

genDependencies :: Gen [Dependency]
genDependencies = Gen.list (Range.linear 1 5) genDependency

genDependency :: Gen Dependency
genDependency = do
  name <- genPackageName
  
  ver <- Gen.maybe genVersionConstraint
  return $ Dependency name ver BuildDepends

genPackageName :: Gen PackageName
genPackageName = do
  first <- Gen.alpha
  rest <- Gen.text (Range.linear 1 10) Gen.alphaNum
  return $ unsafeMkPackageName (T.cons first rest)

genVersionConstraint :: Gen VersionConstraint
genVersionConstraint = Gen.choice
  [ pure AnyVersion
  , ExactVersion <$> genVersion
  , MajorBoundVersion <$> genVersion
  ]

genVersion :: Gen Version
genVersion = do
  nums <- Gen.list (Range.linear 1 3) (Gen.int (Range.linear 0 10))
  return $ Version nums

showVer :: Version -> Text
showVer (Version nums) = T.intercalate "." (map (T.pack . show) nums)


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