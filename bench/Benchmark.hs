{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Core.Parser
import Core.Serializer
import Core.Types
import Business.SetVersion
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ bgroup "parser"
      [ bench "scan common stanzas" $ nf scanCommonStanzas complexContent
      , bench "find conditional position" $ nf (findConditionalPosition "os(windows)") complexContent
      , bench "parse common deps" $ nf (parseCommonDeps complexContent) (TextSpan 300 500)
      ]
  , bgroup "serializer"
      [ bench "insert dependency (leading comma)" $ nf (insertDependencyLine "\n" True sampleDep) sampleContent
      , bench "insert dependency (trailing comma)" $ nf (insertDependencyLine "\n" False sampleDep) sampleContent
      , bench "insert duplicate dependency" $ nf (insertDependencyLine "\n" True duplicateDep) sampleContent
      , bench "remove dependency" $ nf (removeDependencyLine "\n" True sampleDep) sampleContent
      , bench "update project version" $ nf (updateProjectVersion "1.2.3.4") complexContent
      ]
  , bgroup "targeting"
      [ bench "resolve target bounds (conditional)" $ nf (resolveTargetBounds (TargetConditional TargetLib "os(windows)") dummyCabal) complexContent
      ]
  ]

sampleDep :: Dependency
sampleDep = Dependency (unsafeMkPackageName "new-package") (Just (MajorBoundVersion (Version [1,2,3]))) BuildDepends

duplicateDep :: Dependency
duplicateDep = Dependency (unsafeMkPackageName "base") Nothing Nothing BuildDepends

dummyCabal :: CabalFile
dummyCabal = CabalFile 
  { cfPackageName = unsafeMkPackageName "test"
  , cfSections = [LibrarySection (Library Nothing [] (TextSpan 0 1000))]
  , cfRawContent = ""
  , cfLineEndings = "\n"
  }

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