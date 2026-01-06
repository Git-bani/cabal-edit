{-# LANGUAGE OverloadedStrings #-}
module Core.AST.EditorOutlierSpec (spec) where

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
spec = describe "Core.AST.Editor (Outliers)" $ do
  
  describe "Conditionals" $ do
    it "adds dependency to existing if-block" $ do
      let input = T.unlines 
            [ "library"
            , "  if os(windows)"
            , "    build-depends: Win32"
            ]
          dep = mkDep "kernel32" ""
          ast = parseAST input
          result = addDependencyToAST "library" (Just "os(windows)") dep ast
      
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            T.unpack output `shouldContain` "Win32"
            T.unpack output `shouldContain` "kernel32"
        Failure err -> expectationFailure (show err)

    it "creates nested if-block if needed (not supported yet, checking behavior)" $ do
      -- Current implementation likely appends a new if block if not found in top level of section
      let input = T.unlines 
            [ "library"
            , "  build-depends: base"
            ]
          dep = mkDep "Win32" ""
          ast = parseAST input
          result = addDependencyToAST "library" (Just "os(windows)") dep ast
          
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            T.unpack output `shouldContain` "if os(windows)"
            T.unpack output `shouldContain` "build-depends: Win32"
        Failure err -> expectationFailure (show err)

  describe "Comments" $ do
    it "preserves comments inside build-depends list when adding" $ do
      let input = T.unlines
            [ "library"
            , "  build-depends:"
            , "    base,"
            , "    -- Important comment"
            , "    text"
            ]
          dep = mkDep "containers" ""
          ast = parseAST input
          result = addDependencyToAST "library" Nothing dep ast
          
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            T.unpack output `shouldContain` "-- Important comment"
            T.unpack output `shouldContain` "containers"
        Failure err -> expectationFailure (show err)

    it "preserves comments inside build-depends list when removing" $ do
      let input = T.unlines
            [ "library"
            , "  build-depends:"
            , "    base,"
            , "    -- Keep this"
            , "    text"
            ]
          ast = parseAST input
          result = removeDependencyFromAST "library" Nothing "text" ast
          
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            T.unpack output `shouldContain` "-- Keep this"
            T.unpack output `shouldContain` "base"
            T.unpack output `shouldNotContain` "text"
        Failure err -> expectationFailure (show err)

  describe "Empty Blocks" $ do
    it "handles removing last item from conditional block" $ do
      let input = T.unlines
            [ "library"
            , "  if flag(dev)"
            , "    build-depends: text"
            ]
          ast = parseAST input
          result = removeDependencyFromAST "library" (Just "flag(dev)") "text" ast
          
      case result of
        Success newAst -> do
            let output = serializeAST newAst
            -- Should remove the empty if block entirely? 
            -- Or leave empty block? Editor.hs seems to try to remove empty blocks.
            T.unpack output `shouldNotContain` "if flag(dev)"
        Failure err -> expectationFailure (show err)
