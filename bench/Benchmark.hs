{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Core.AST.Parser
import Core.AST.Serializer
import Core.AST.Editor
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ bgroup "AST Parser"
      [ bench "parse sample content" $ nf parseAST sampleContent
      , bench "parse complex content" $ nf parseAST complexContent
      ]
  , bgroup "AST Serializer"
      [ bench "serialize sample AST" $ nf (serializeAST . parseAST) sampleContent
      , bench "serialize complex AST" $ nf (serializeAST . parseAST) complexContent
      ]
  , bgroup "AST Editor"
      [ bench "add dependency" $ nf (addDependencyToAST "library" Nothing sampleDep) (parseAST sampleContent)
      , bench "remove dependency" $ nf (removeDependencyFromAST "library" Nothing "text") (parseAST sampleContent)
      , bench "update project version" $ nf (updateFieldInAST "version" "1.2.3.4") (parseAST complexContent)
      ]
  ]

sampleDep :: Dependency
sampleDep = Dependency (trustedMkPackageName "new-package") (Just (MajorBoundVersion (Version [1,2,3]))) BuildDepends

sampleContent :: Text
sampleContent = T.unlines
  [ "cabal-version: 2.4"
  , "name: sample"
  , "version: 0.1.0.0"
  , "library"
  , "  build-depends: base"
  , "               , text"
  ]

complexContent :: Text
complexContent = T.unlines
  [ "cabal-version: 3.0"
  , "name: complex"
  , "version: 0.1.0.0"
  , ""
  , "common shared"
  , "    build-depends: base >= 4.14"
  , ""
  , "library"
  , "    import: shared"
  , "    if os(windows)"
  , "        build-depends: Win32"
  , "    if os(linux)"
  , "        build-depends: unix"
  ]
