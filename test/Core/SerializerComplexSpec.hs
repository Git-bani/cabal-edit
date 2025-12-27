{-# LANGUAGE OverloadedStrings #-}

module Core.SerializerComplexSpec (spec) where

import Test.Hspec
import Core.Serializer
import Core.Types
import qualified Data.Text as T

spec :: Spec
spec = describe "Core.Serializer (Complex)" $ do

  it "adds dependency to main block ignoring if-blocks" $ do
    let content = T.unlines
          [ "library"
          , "  exposed-modules: Lib"
          , "  build-depends: base, text"
          , ""
          , "  if flag(dev)"
          , "    build-depends: extra"
          ]
    
    let dep = Dependency (unsafeMkPackageName "aeson") Nothing (Just (ExactVersion (Version [2,0]))) BuildDepends
    let result = insertDependencyLine "\n" True dep content
    
    -- It should add to the first build-depends (main block)
    T.unpack result `shouldContain` "aeson ==2.0"
    -- It should be in the main block (before "if flag(dev)")
    let (mainBlock, ifBlock) = T.breakOn "if flag(dev)" result
    T.unpack mainBlock `shouldContain` "aeson ==2.0"
    T.unpack ifBlock `shouldNotContain` "aeson ==2.0"

  it "adds dependency to main block when main block has NO build-depends but if-block DOES" $ do
    let content = T.unlines
          [ "library"
          , "  exposed-modules: Lib"
          , ""
          , "  if flag(dev)"
          , "    build-depends: extra"
          ]
    
    let dep = Dependency (unsafeMkPackageName "aeson") Nothing (Just (ExactVersion (Version [2,0]))) BuildDepends
    let result = insertDependencyLine "\n" True dep content
    
    -- Current behavior check:
    -- If it finds "build-depends" in the if-block, does it append there?
    -- Ideally, it should create a NEW build-depends in the main block.
    
    T.unpack result `shouldContain` "aeson ==2.0"
    
    -- Verify position:
    -- We want it BEFORE "if flag(dev)"
    let (beforeIf, _) = T.breakOn "if flag(dev)" result
    
    -- If it inserted into the if-block, beforeIf won't contain it.
    -- If it correctly created a new field, beforeIf SHOULD contain it.
    T.unpack beforeIf `shouldContain` "aeson ==2.0"

  it "handles common stanzas correctly" $ do
    -- We are simulating passing just the "library" section content to insertDependencyLine
    -- (The caller 'Business.Add' splits the file and passes specific section content)
    let sectionContent = T.unlines
          [ "  import: shared"
          , "  -- Main lib"
          , "  build-depends: text"
          ]
          
    let dep = Dependency (unsafeMkPackageName "aeson") Nothing (Just (ExactVersion (Version [2,0]))) BuildDepends
    let result = insertDependencyLine "\n" True dep sectionContent
    
    T.unpack result `shouldContain` "aeson ==2.0"
    T.unpack result `shouldContain` "import: shared"