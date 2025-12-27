{-# LANGUAGE OverloadedStrings #-}

module Core.TrailingCommaSpec (spec) where

import Test.Hspec
import Core.Serializer
import Core.Types
import qualified Data.Text as T

spec :: Spec
spec = describe "Trailing Comma Support" $ do
  
  it "formats dependency list with trailing commas" $ do
    let deps = [ Dependency (unsafeMkPackageName "package-a") Nothing Nothing BuildDepends
               , Dependency (unsafeMkPackageName "package-b") Nothing Nothing BuildDepends
               ]
    -- Use leadingComma = False
    let result = formatDependencyList "\n" False deps 2
    T.unpack result `shouldContain` "package-a,"
    T.unpack result `shouldContain` "package-b"
    T.unpack result `shouldNotContain` ", package-b"

  it "inserts new dependency with trailing comma style" $ do
    let content = T.unlines
          ["library"
          , "  build-depends:"
          , "    base,"
          ]
    let dep = Dependency (unsafeMkPackageName "text") Nothing Nothing BuildDepends
    let result = insertDependencyLine "\n" False dep content
    
    T.unpack result `shouldContain` "base,"
    T.unpack result `shouldContain` "text"
    -- The new line should have "text" and NO leading comma
    T.unpack result `shouldNotContain` ", text"
    
  it "handles adding first dependency in trailing comma style" $ do
    let content = "library\n  build-depends:\n"
    let dep = Dependency (unsafeMkPackageName "base") Nothing Nothing BuildDepends
    let result = insertDependencyLine "\n" False dep content
    
    -- Should not have any comma if it's the only one
    T.unpack result `shouldContain` "base"
    T.unpack result `shouldNotContain` ","
