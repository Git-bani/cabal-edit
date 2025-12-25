{-# LANGUAGE OverloadedStrings #-}

module Business.ValidationSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Business.Validation
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Business.Validation" $ do
  
  describe "validatePackageName" $ do
    it "accepts valid package names" $ hedgehog $ do
      name <- forAll genValidPackageName
      validatePackageName name === Success ()

    it "rejects empty names" $ do
      let result = validatePackageName ""
      case result of
        Failure (Error msg _) -> T.unpack msg `shouldContain` "cannot be empty"
        _ -> expectationFailure "Should have failed"

    it "rejects names starting/ending with hyphens" $ hedgehog $ do
      name <- forAll genValidPackageName
      let invalidStart = "-" <> name
      let invalidEnd = name <> "-"
      
      let res1 = validatePackageName invalidStart
      let res2 = validatePackageName invalidEnd
      
      isFailure res1 === True
      isFailure res2 === True

    it "rejects names with consecutive hyphens" $ hedgehog $ do
      name <- forAll genValidPackageName
      let invalid = name <> "--" <> name
      let res = validatePackageName invalid
      isFailure res === True
      
    it "rejects names with invalid characters" $ hedgehog $ do
      name <- forAll genValidPackageName
      invalidChar <- forAll $ Gen.element ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', ' ']
      let invalid = name <> T.singleton invalidChar <> name
      let res = validatePackageName invalid
      isFailure res === True

-- Generators

genValidPackageName :: Gen Text
genValidPackageName = do
  -- Generate list of segments (non-empty alphanumeric strings)
  -- Package names are segments separated by hyphens
  segments <- Gen.list (Range.linear 1 5) genSegment
  return $ T.intercalate "-" segments

genSegment :: Gen Text
genSegment = Gen.text (Range.linear 1 10) Gen.alphaNum

isFailure :: Result a -> Bool
isFailure (Failure _) = True
isFailure _ = False
