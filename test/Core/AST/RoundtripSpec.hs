{-# LANGUAGE OverloadedStrings #-}
module Core.AST.RoundtripSpec (spec) where

import Test.Hspec
import qualified Data.Text.IO as TIO

import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)

spec :: Spec
spec = describe "Core.AST.Roundtrip" $ do
  
  it "roundtrips complex.cabal exactly" $ do
    content <- TIO.readFile "test/Golden/fixtures/complex.cabal"
    let ast = parseAST content
    let generated = serializeAST ast
    generated `shouldBe` content

  it "roundtrips common-stanzas.cabal exactly" $ do
    content <- TIO.readFile "test/Golden/fixtures/common-stanzas.cabal"
    let ast = parseAST content
    let generated = serializeAST ast
    generated `shouldBe` content
