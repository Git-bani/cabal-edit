{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Core.AST.Parser
import Core.AST.Editor
import Core.Types
import Utils.Diff
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ bgroup "AST Parser"
      [ bench "parse sample content" $ nf parseAST sampleContent
      , bench "parse complex content" $ nf parseAST complexContent
      ]
  , bgroup "AST Editor"
      [ bench "add dependency" $ nf (addDependencyToAST "library" Nothing sampleDep) (parseAST sampleContent)
      , bench "remove dependency" $ nf (removeDependencyFromAST "library" Nothing "text") (parseAST sampleContent)
      ]
  , bgroup "Scaling (O(N))"
      [ bench "parse 100 deps" $ nf parseAST (massiveContent 100)
      , bench "parse 1000 deps" $ nf parseAST (massiveContent 1000)
      , bench "parse 5000 deps" $ nf parseAST (massiveContent 5000)
      , bench "add to 5000 deps" $ nf (addDependencyToAST "library" Nothing sampleDep) (parseAST (massiveContent 5000))
      ]
  , bgroup "Diff Engine"
      [ bench "diff 100 lines (small change)" $ 
          let c1 = massiveContent 100
              c2 = massiveContent 101
          in nf (diffLines (T.lines c1)) (T.lines c2)
      , bench "diff 1000 lines (small change)" $ 
          let c1 = massiveContent 1000
              c2 = massiveContent 1001
          in nf (diffLines (T.lines c1)) (T.lines c2)
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

massiveContent :: Int -> Text
massiveContent n = T.unlines $
  [ "cabal-version: 3.0"
  , "name: massive"
  , "version: 1.0"
  , "library"
  , "  build-depends: base"
  ] ++ [ "               , dep-" <> T.pack (show i) | i <- [1..n] ]