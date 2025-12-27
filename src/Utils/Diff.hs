{-# LANGUAGE OverloadedStrings #-}
module Utils.Diff (diffLines, colorizeDiff, Diff(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI

data Diff a = Both a | First a | Second a
  deriving (Show, Eq)

diffLines :: Eq a => [a] -> [a] -> [Diff a]
diffLines xs ys = 
  let (pre, xs', ys') = stripCommonPrefix xs ys
      (suf, xs'', ys'') = stripCommonSuffix xs' ys'
      middle = lcs xs'' ys''
  in map Both pre ++ middle ++ map Both suf

stripCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
stripCommonPrefix (x:xs) (y:ys)
  | x == y = let (p, xs', ys') = stripCommonPrefix xs ys in (x:p, xs', ys')
stripCommonPrefix xs ys = ([], xs, ys)

stripCommonSuffix :: Eq a => [a] -> [a] -> ([a], [a], [a])
stripCommonSuffix xs ys = 
  let (p, rx, ry) = stripCommonPrefix (reverse xs) (reverse ys)
  in (reverse p, reverse rx, reverse ry)

lcs :: Eq a => [a] -> [a] -> [Diff a]
lcs [] [] = []
lcs xs [] = map First xs
lcs [] ys = map Second ys
lcs (x:xs) (y:ys)
  | x == y    = Both x : lcs xs ys
  | otherwise = 
      let left = lcs xs (y:ys) -- Skip x
          right = lcs (x:xs) ys -- Skip y
      in if lengthDiff left <= lengthDiff right then First x : left else Second y : right

lengthDiff :: [Diff a] -> Int
lengthDiff = length

colorizeDiff :: [Diff Text] -> IO ()
colorizeDiff diffs = do
  mapM_ printDiff diffs
  setSGR [Reset]
  where
    printDiff (Both t) = do
      setSGR [SetColor Foreground Dull White] -- Dim/Normal
      TIO.putStrLn $ "  " <> t
    printDiff (First t) = do
      setSGR [SetColor Foreground Vivid Red]
      TIO.putStrLn $ "- " <> t
    printDiff (Second t) = do
      setSGR [SetColor Foreground Vivid Green]
      TIO.putStrLn $ "+ " <> t
