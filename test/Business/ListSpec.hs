{-# LANGUAGE OverloadedStrings #-}

module Business.ListSpec (spec) where
import Data.Either (isRight, isLeft)

import Test.Hspec
import Business.List (formatDependenciesAST)
import Core.Types
import Core.AST.Parser (parseAST)
import qualified Data.Text as T

spec :: Spec
spec = describe "Business.List" $ do
  
  describe "formatDependenciesAST" $ do
    it "lists dependencies for a simple library" $ do
      let content = "library\n  build-depends: base >=4.14, text >=1.2"
      let ast = parseAST content
      let output = formatDependenciesAST (ListOptions False) ast
      
      output `shouldSatisfy` ("library:" `T.isInfixOf`)
      output `shouldSatisfy` ("  - base >=4.14" `T.isInfixOf`)
      output `shouldSatisfy` ("  - text >=1.2" `T.isInfixOf`)

    it "lists dependencies for multiple sections" $ do
      let content = T.unlines
            [ "library"
            , "  build-depends: base"
            , "executable my-exe"
            , "  build-depends: text"
            , "test-suite my-test"
            , "  build-depends: base"
            ]
      let ast = parseAST content
      let output = formatDependenciesAST (ListOptions False) ast
      
      output `shouldSatisfy` ("library:" `T.isInfixOf`)
      output `shouldSatisfy` ("executable my-exe:" `T.isInfixOf`)
      output `shouldSatisfy` ("test-suite my-test:" `T.isInfixOf`)

    it "sorts dependencies alphabetically" $ do
      let content = "library\n  build-depends: text >=1.2, base >=4.14"
      let ast = parseAST content
      let output = formatDependenciesAST (ListOptions False) ast
      
      let lines' = filter ("  - " `T.isPrefixOf`) (T.lines output)
      lines' `shouldBe` ["  - base >=4.14", "  - text >=1.2"]
