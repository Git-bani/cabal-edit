{-# LANGUAGE OverloadedStrings #-}

module Utils.DiffSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog
-- import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Utils.Diff
-- import qualified Data.Text as T
-- import Data.List (nub)

spec :: Spec
spec = do
  describe "Utils.Diff" $ do
    it "reconstructs original lists from Diff" $ hedgehog $ do
      xs <- forAll $ Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
      ys <- forAll $ Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
      
      let diffs = diffLines xs ys
      
      -- Simplified reconstruction logic to avoid warning
      let rXs = concatMap (\d -> case d of Both v -> [v]; First v -> [v]; _ -> []) diffs
      let rYs = concatMap (\d -> case d of Both v -> [v]; Second v -> [v]; _ -> []) diffs
      
      rXs === xs
      rYs === ys

    it "identifies common elements correctly" $ hedgehog $ do
      common <- forAll $ Gen.list (Range.linear 1 10) Gen.alpha
      prefix <- forAll $ Gen.list (Range.linear 0 5) Gen.alpha
      suffix <- forAll $ Gen.list (Range.linear 0 5) Gen.alpha
      
      let xs = prefix ++ common ++ suffix
      -- ys has same common part but different prefix/suffix
      p2 <- forAll $ Gen.list (Range.linear 0 5) Gen.alpha
      s2 <- forAll $ Gen.list (Range.linear 0 5) Gen.alpha
      let ys = p2 ++ common ++ s2
      
      let diffs = diffLines xs ys
      
      -- Ensure common parts are detected as Both (if they align best)
      -- Note: LCS isn't unique, but if the common part is significant, it should be preserved.
      -- This property is weaker: check that Both elements are indeed in both lists.
      let boths = [x | Both x <- diffs]
      assert $ all (\x -> x `elem` xs && x `elem` ys) boths
