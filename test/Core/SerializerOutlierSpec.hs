{-# LANGUAGE OverloadedStrings #-}

module Core.SerializerOutlierSpec (spec) where

import Test.Hspec
import Core.Serializer
import Core.Types
import qualified Data.Text as T

spec :: Spec
spec = describe "Core.Serializer (Outliers)" $ do

  describe "insertDependencyLine" $ do
    it "handles file with no newline at EOF" $ do
      let content = "library\n  build-depends: base" -- No newline
      let dep = Dependency (unsafeMkPackageName "text") Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      -- Should append correctly without merging lines
      T.unpack result `shouldContain` "base"
      T.unpack result `shouldContain` "text"
      -- The result probably adds a newline, which is fine/good.

    it "handles completely empty build-depends" $ do
      let content = "library\n  build-depends:"
      let dep = Dependency (unsafeMkPackageName "text") Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      T.unpack result `shouldContain` "text"

    it "handles CRLF line endings (preserves or normalizes?)" $ do
      -- Current implementation splits by '\n', so CRLF becomes "line\r".
      -- This might be tricky. Let's see behavior.
      let content = "library\r\n  build-depends: base\r\n"
      let dep = Dependency (unsafeMkPackageName "text") Nothing BuildDepends
      let result = insertDependencyLine "\r\n" True dep content
      
      T.unpack result `shouldContain` "text"
      -- Ideally should preserve CRLF but mixed is okay for now as long as it's valid.

    it "inserts into section with comments only" $ do
      let content = "library\n  -- Just a comment\n"
      let dep = Dependency (unsafeMkPackageName "text") Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      
      T.unpack result `shouldContain` "build-depends:"
      T.unpack result `shouldContain` "text"
      -- Ensure comment is preserved
      T.unpack result `shouldContain` "-- Just a comment"

  describe "replaceBuildDependsBlock" $ do
    it "replaces block when only one dependency exists" $ do
      let content = "library\n  build-depends: base"
      let newDeps = [Dependency (unsafeMkPackageName "text") Nothing BuildDepends]
      let result = replaceBuildDependsBlock "\n" True newDeps content
      
      T.unpack result `shouldContain` "text"
      T.unpack result `shouldNotContain` "base"

    it "replaces block keeping surrounding whitespace" $ do
      let content = "library\n\n  build-depends: base\n\n  default-language: Haskell2010"
      let newDeps = [Dependency (unsafeMkPackageName "text") Nothing BuildDepends]
      let result = replaceBuildDependsBlock "\n" True newDeps content
      
      -- Should still have blank lines
      T.unpack result `shouldContain` "\n\n  build-depends:"
      T.unpack result `shouldContain` "\n\n  default-language:"

  describe "updateDependencyLine (Outliers)" $ do
    it "updates dependency with sub-library correctly" $ do
      let content = "library\n  build-depends: my-pkg:sublib == 1.0"
      let dep = Dependency (unsafeMkPackageName "my-pkg:sublib") (Just (ExactVersion (Version [2,0]))) BuildDepends
      let result = updateDependencyLine "\n" True dep content
      T.unpack result `shouldContain` "my-pkg:sublib ==2.0"
      T.unpack result `shouldNotContain` "1.0"

    it "handles dependency name that is a prefix of another" $ do
      -- This tests that updating 'text' doesn't accidentally update 'text-conversions'
      let content = "library\n  build-depends: text-conversions == 0.1, text == 1.2"
      let dep = Dependency (unsafeMkPackageName "text") (Just (ExactVersion (Version [2,0]))) BuildDepends
      let result = updateDependencyLine "\n" True dep content
      T.unpack result `shouldContain` "text ==2.0"
      T.unpack result `shouldContain` "text-conversions ==0.1"

