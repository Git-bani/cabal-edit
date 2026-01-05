{-# LANGUAGE OverloadedStrings #-}
module Core.AST.EditorSpec (spec) where

import Test.Hspec
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (addDependencyToAST, removeDependencyFromAST)
import Core.Types (Dependency(..), Result(..), VersionConstraint(..), DependencyType(..), trustedMkPackageName)
import qualified Data.Text as T

mkDep :: T.Text -> T.Text -> Dependency
mkDep name ver = Dependency 
  { depName = trustedMkPackageName name
  , depVersionConstraint = if T.null ver then Nothing else Just (UnparsedVersion ver)
  , depType = BuildDepends
  }

spec :: Spec
spec = describe "Core.AST.Editor" $ do
  
  describe "addDependencyToAST" $ do
    it "adds a dependency to a single-line build-depends" $ do
      let input = "library\n  build-depends: base"
          dep = mkDep "text" ">=1.2"
          ast = parseAST input
          result = addDependencyToAST "library" Nothing dep ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            output `shouldBe` "library\n  build-depends: base, text >=1.2"
        Failure err -> expectationFailure (show err)

    it "adds a dependency to a multi-line build-depends (trailing style)" $ do
      let input = "library\n  build-depends:\n      base,\n      containers"
          dep = mkDep "text" ">=1.2"
          ast = parseAST input
          result = addDependencyToAST "library" Nothing dep ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            T.unpack output `shouldContain` "containers,\n      text >=1.2"
        Failure err -> expectationFailure (show err)

    it "adds a dependency to a multi-line build-depends (leading style)" $ do
      let input = "library\n  build-depends:\n      base\n    , containers"
          dep = mkDep "text" ">=1.2"
          ast = parseAST input
          result = addDependencyToAST "library" Nothing dep ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            T.unpack output `shouldContain` "containers\n    , text >=1.2"
        Failure err -> expectationFailure (show err)

    it "updates an existing dependency version" $ do
      let input = "library\n  build-depends: base, text >=1.0"
          dep = mkDep "text" ">=2.0"
          ast = parseAST input
          result = addDependencyToAST "library" Nothing dep ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            output `shouldBe` "library\n  build-depends: base, text >=2.0"
        Failure err -> expectationFailure (show err)

    it "fails if the section is not found" $ do
      let input = "library\n  build-depends: base"
          dep = mkDep "text" ">=1.2"
          ast = parseAST input
          result = addDependencyToAST "executable app" Nothing dep ast
      case result of
        Failure _ -> return ()
        Success _ -> expectationFailure "Should have failed"

  describe "removeDependencyFromAST" $ do
    it "removes a dependency from single-line build-depends" $ do
      let input = "library\n  build-depends: base, text >=1.2"
          ast = parseAST input
          result = removeDependencyFromAST "library" Nothing "text" ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            output `shouldBe` "library\n  build-depends: base"
        Failure err -> expectationFailure (show err)

    it "removes a dependency from multi-line build-depends" $ do
      let input = "library\n  build-depends:\n    base,\n    text"
          ast = parseAST input
          result = removeDependencyFromAST "library" Nothing "text" ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            -- Check that structure is preserved (newline kept)
            T.unpack output `shouldContain` "build-depends:\n    base"
            T.unpack output `shouldNotContain` "text"
        Failure err -> expectationFailure (show err)

    it "removes the last dependency correctly" $ do
      let input = "library\n  build-depends: text"
          ast = parseAST input
          result = removeDependencyFromAST "library" Nothing "text" ast
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            output `shouldBe` "library\n"
        Failure err -> expectationFailure (show err)
