{-# LANGUAGE OverloadedStrings #-}

module Core.DependencyResolverSpec (spec) where

import Test.Hspec
import Core.DependencyResolver
import Core.Types

spec :: Spec
spec = describe "Core.DependencyResolver" $ do
  
  describe "resolveVersionConstraint" $ do
    it "preserves user input as UnparsedVersion" $ do
      result <- resolveVersionConstraint (unsafeMkPackageName "pkg") (Just "^>= 1.2.3")
      result `shouldBe` Success (UnparsedVersion "^>= 1.2.3")

    it "preserves constraints with operators" $ do
      result <- resolveVersionConstraint (unsafeMkPackageName "pkg") (Just "==1.2.3")
      result `shouldBe` Success (UnparsedVersion "==1.2.3")

    it "preserves complex constraints without modification" $ do
      result <- resolveVersionConstraint (unsafeMkPackageName "pkg") (Just ">= 1.2")
      result `shouldBe` Success (UnparsedVersion ">= 1.2")

    it "rejects invalid version constraints" $ do
      result <- resolveVersionConstraint (unsafeMkPackageName "pkg") (Just ">>== invalid")
      case result of
        Failure (Error _ InvalidDependency) -> return ()
        _ -> expectationFailure $ "Should have rejected invalid constraint, got: " ++ show result

      result2 <- resolveVersionConstraint (unsafeMkPackageName "pkg") (Just "foo")
      case result2 of
        Failure (Error _ InvalidDependency) -> return ()
        _ -> expectationFailure $ "Should have rejected 'foo', got: " ++ show result2

    it "returns MajorBoundVersion (^>=) when no version is provided" $ do
      -- This hits the network/cache
      result <- resolveVersionConstraint (unsafeMkPackageName "aeson") Nothing
      case result of
        Success (MajorBoundVersion _) -> return ()
        _ -> expectationFailure $ "Should have returned MajorBoundVersion, got: " ++ show result
