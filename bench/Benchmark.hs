{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Core.Parser
import Core.Serializer
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ bgroup "parser"
      [ bench "parse sample" $ nfIO (parseCabalFile "cabal-edit.cabal")
      ]
  , bgroup "serializer"
      [ bench "insert dependency" $ nf insertDep sampleContent
      ]
  ]

insertDep :: Text -> Text
insertDep content = 
  let dep = Dependency (unsafeMkPackageName "new-package") Nothing BuildDepends
  in insertDependencyLine dep content

sampleContent :: Text
sampleContent = T.unlines
  [ "cabal-version: 2.4"
  , "name: sample"
  , "version: 0.1.0.0"
  , "library"
  , "  build-depends: base"
  , "               , text"
  ]
