{-# LANGUAGE OverloadedStrings #-}

module Business.List (listDependencies, formatDependencies) where

import Core.Types
import Core.Parser (parseCabalFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)
import Control.Monad (forM_)
import qualified Distribution.Pretty as DP

listDependencies :: ListOptions -> FilePath -> IO (Result ())
listDependencies opts path = do
  parseResult <- parseCabalFile path
  
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      let output = formatDependencies opts cabalFile
      TIO.putStrLn output
      return $ Success ()

formatDependencies :: ListOptions -> CabalFile -> T.Text
formatDependencies _ cf = T.intercalate "\n" $ filter (not . T.null) lines'
  where
    lines' = ("Dependencies for " <> unPackageName (cfPackageName cf)) : concatMap formatSection (cfSections cf)

formatSection :: Section -> [T.Text]
formatSection section = case section of
  LibrarySection lib -> 
    fmtSec (maybe "Library" ("Library " <>) (libName lib)) (libBuildDepends lib)
  ExecutableSection exe -> 
    fmtSec ("Executable " <> exeName exe) (exeBuildDepends exe)
  TestSuiteSection test -> 
    fmtSec ("Test Suite " <> testName test) (testBuildDepends test)
  BenchmarkSection bench -> 
    fmtSec ("Benchmark " <> benchName bench) (benchBuildDepends bench)
  CommonStanzaSection common ->
    fmtSec ("Common Stanza " <> commonName common) (commonBuildDepends common)
  _ -> []

fmtSec :: T.Text -> [Dependency] -> [T.Text]
fmtSec name deps
  | null deps = []
  | otherwise = 
      let header = "\n" <> name <> ":"
          depLines = map fmtDep (sort deps)
      in header : depLines

fmtDep :: Dependency -> T.Text
fmtDep dep = 
  let ver = formatVersionConstraint (depVersionConstraint dep)
  in "  - " <> unPackageName (depName dep) <> " " <> ver

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
