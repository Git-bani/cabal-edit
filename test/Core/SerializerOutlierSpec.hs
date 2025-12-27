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
      let dep = Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      -- Should append correctly without merging lines
      T.unpack result `shouldContain` "base"
      T.unpack result `shouldContain` "text"
      -- The result probably adds a newline, which is fine/good.

    it "handles completely empty build-depends" $ do
      let content = "library\n  build-depends:"
      let dep = Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      T.unpack result `shouldContain` "text"

    it "handles CRLF line endings (preserves or normalizes?)" $ do
      -- Current implementation splits by '\n', so CRLF becomes "line\r".
      -- This might be tricky. Let's see behavior.
      let content = "library\r\n  build-depends: base\r\n"
      let dep = Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends
      let result = insertDependencyLine "\r\n" True dep content
      
      T.unpack result `shouldContain` "text"
      -- Ideally should preserve CRLF but mixed is okay for now as long as it's valid.

    it "inserts into section with comments only" $ do
      let content = "library\n  -- Just a comment\n"
      let dep = Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      
      T.unpack result `shouldContain` "build-depends:"
      T.unpack result `shouldContain` "text"
      -- Ensure comment is preserved
      T.unpack result `shouldContain` "-- Just a comment"

  describe "replaceBuildDependsBlock" $ do
    it "replaces block when only one dependency exists" $ do
      let content = "library\n  build-depends: base"
      let newDeps = [Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends]
      let result = replaceBuildDependsBlock "\n" True newDeps content
      
      T.unpack result `shouldContain` "text"
      T.unpack result `shouldNotContain` "base"

    it "replaces block keeping surrounding whitespace" $ do
      let content = "library\n\n  build-depends: base\n\n  default-language: Haskell2010"
      let newDeps = [Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends]
      let result = replaceBuildDependsBlock "\n" True newDeps content
      
      -- Should still have blank lines
      T.unpack result `shouldContain` "\n\n  build-depends:"
      T.unpack result `shouldContain` "\n\n  default-language:"
