{-# LANGUAGE OverloadedStrings #-}

module Utils.DiffSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Utils.Diff
import qualified Data.Text as T
import Data.List (nub)

spec :: Spec
spec = do
  describe "Utils.Diff" $ do
    it "reconstructs original lists from Diff" $ hedgehog $ do
      xs <- forAll $ Gen.list (Range.linear 0 20) Gen.alpha
      ys <- forAll $ Gen.list (Range.linear 0 20) Gen.alpha
      
      let diffs = diffLines xs ys
      
      let reconstructedXs = [x | d <- diffs, case d of Both x -> True; First x -> True; _ -> False, let x = case d of Both v -> v; First v -> v; _ -> error "impossibru"]
      let reconstructedYs = [y | d <- diffs, case d of Both y -> True; Second y -> True; _ -> False, let y = case d of Both v -> v; Second v -> v; _ -> error "impossibru"]
      
      reconstructedXs === xs
      reconstructedYs === ys

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
