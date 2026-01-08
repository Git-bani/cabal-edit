{-# LANGUAGE OverloadedStrings #-}

module Core.DependencyResolverSpec (spec) where
import Data.Either (isRight, isLeft)

import Test.Hspec
import Core.DependencyResolver
import Core.Types

spec :: Spec
spec = describe "Core.DependencyResolver" $ do
  
  describe "resolveVersionConstraint" $ do
    it "preserves user input as UnparsedVersion" $ do
      result <- resolveVersionConstraint Nothing Nothing (trustedMkPackageName "pkg") (Just "^>= 1.2.3")
      result `shouldBe` Right (UnparsedVersion "^>= 1.2.3")

    it "preserves constraints with operators" $ do
      result <- resolveVersionConstraint Nothing Nothing (trustedMkPackageName "pkg") (Just "==1.2.3")
      result `shouldBe` Right (UnparsedVersion "==1.2.3")

    it "preserves complex constraints without modification" $ do
      result <- resolveVersionConstraint Nothing Nothing (trustedMkPackageName "pkg") (Just ">= 1.2")
      result `shouldBe` Right (UnparsedVersion ">= 1.2")

    it "rejects invalid version constraints" $ do
      result <- resolveVersionConstraint Nothing Nothing (trustedMkPackageName "pkg") (Just ">>== invalid")
      case result of
        Left (Error _ InvalidDependency) -> return ()
        _ -> expectationFailure $ "Should have rejected invalid constraint, got: " ++ show result

      result2 <- resolveVersionConstraint Nothing Nothing (trustedMkPackageName "pkg") (Just "foo")
      case result2 of
        Left (Error _ InvalidDependency) -> return ()
        _ -> expectationFailure $ "Should have rejected 'foo', got: " ++ show result2

    it "returns MajorBoundVersion (^>=) when no version is provided and Cabal >= 2.0" $ do
      -- This hits the network/cache
      result <- resolveVersionConstraint Nothing (Just (Version [2,4])) (trustedMkPackageName "aeson") Nothing
      case result of
        Right (MajorBoundVersion _) -> return ()
        _ -> expectationFailure $ "Should have returned MajorBoundVersion, got: " ++ show result

    it "returns RangeVersion (>= && <) when no version is provided and Cabal < 2.0" $ do
      -- This hits the network/cache
      result <- resolveVersionConstraint Nothing (Just (Version [1,24])) (trustedMkPackageName "aeson") Nothing
      case result of
        Right (RangeVersion _) -> return ()
        _ -> expectationFailure $ "Should have returned RangeVersion, got: " ++ show result
