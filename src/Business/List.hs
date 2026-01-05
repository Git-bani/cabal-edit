{-# LANGUAGE OverloadedStrings #-}

module Business.List (listDependencies, formatDependenciesAST) where

import Core.Types
import Core.AST.Types (CabalAST)
import Core.AST.Parser (parseAST)
import Core.AST.Editor (findDependenciesInAST)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort, groupBy, sortOn)
import Data.Function (on)
import Control.Monad (forM_)
import qualified Distribution.Pretty as DP

listDependencies :: ListOptions -> FilePath -> IO (Result ())
listDependencies opts path = do
  content <- TIO.readFile path
  let ast = parseAST content
  TIO.putStrLn $ formatDependenciesAST opts ast
  return $ Success ()

formatDependenciesAST :: ListOptions -> CabalAST -> T.Text
formatDependenciesAST _ ast = 
  let deps = findDependenciesInAST ast
  in if null deps
     then "No dependencies found."
     else T.intercalate "\n" $ concatMap formatGroup (groupBy ((==) `on` fst3) deps)
  where
    formatGroup secDeps =
      let (secName, _, _) = head secDeps
          header = "\n" <> secName <> ":"
          sorted = sortOn (depName . thd3) secDeps
          depLines = map formatEntry sorted
      in header : depLines
    
    formatEntry (_, mCond, d) =
      let condStr = case mCond of
                      Just c -> " (if " <> c <> ")"
                      Nothing -> ""
      in "  - " <> unPackageName (depName d) <> " " <> formatVersionConstraint (depVersionConstraint d) <> condStr

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

formatVersionConstraint :: Maybe VersionConstraint -> T.Text
formatVersionConstraint Nothing = "(*)"
formatVersionConstraint (Just vc) = case vc of
  AnyVersion -> "(*)"
  ExactVersion v -> "==" <> showVersion v
  MajorBoundVersion v -> "^>=" <> showVersion v
  RangeVersion _ -> "(range)" -- TODO: Pretty print internal range
  UnparsedVersion t -> t
  CabalVersionRange vr -> T.pack $ DP.prettyShow vr
  WorkspaceVersion -> "(workspace)"

showVersion :: Version -> T.Text
showVersion (Version nums) = T.intercalate "." (map (T.pack . show) nums)
