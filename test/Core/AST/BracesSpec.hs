{-# LANGUAGE OverloadedStrings #-}

module Core.AST.BracesSpec (spec) where

import Test.Hspec
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import qualified Data.Text as T

spec :: Spec
spec = describe "AST Braces Support" $ do
  
  it "roundtrips a section with braces" $ do
    let content = T.unlines
          [ "library {"
          , "  build-depends: base"
          , "}"
          ]
    let ast = parseAST content
    serializeAST ast `shouldBe` content

  it "roundtrips a section with braces and arguments" $ do
    let content = T.unlines
          [ "executable my-exe {"
          , "  main-is: Main.hs"
          , "}"
          ]
    let ast = parseAST content
    serializeAST ast `shouldBe` content

  it "roundtrips if blocks with braces" $ do
    let content = T.unlines
          [ "library"
          , "  if os(windows) {"
          , "    build-depends: Win32"
          , "  }"
          ]
    let ast = parseAST content
    serializeAST ast `shouldBe` content

  it "roundtrips if-else blocks with braces" $ do
    let content = T.unlines
          [ "library"
          , "  if os(windows) {"
          , "    build-depends: Win32"
          , "  } else {"
          , "    build-depends: unix"
          , "  }"
          ]
    let ast = parseAST content
    -- Note: My serializer might be opinionated about closing brace placement
    -- Let's see what it produces.
    serializeAST ast `shouldBe` content

  it "handles mixed indentation and braces" $ do
    let content = T.unlines
          [ "library {"
          , "build-depends: base"
          , "}"
          , "executable foo"
          , "  main-is: Foo.hs"
          ]
    let ast = parseAST content
    serializeAST ast `shouldBe` content
