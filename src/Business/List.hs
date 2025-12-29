{-# LANGUAGE OverloadedStrings #-}

module Business.List (listDependencies) where

import Core.Types
import Core.Parser (parseCabalFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)
import Control.Monad (forM_)
import qualified Distribution.Pretty as DP

listDependencies :: ListOptions -> FilePath -> IO (Result ())
listDependencies opts path = do
  -- We assume path is a .cabal file. If Main.hs passes package.yaml, we should have handled it or handle it here.
  -- Ideally Main.hs resolves to the .cabal file for this command.
  parseResult <- parseCabalFile path
  
  case parseResult of
    Failure err -> return $ Failure err
    Success cabalFile -> do
      printDependencies opts cabalFile
      return $ Success ()

printDependencies :: ListOptions -> CabalFile -> IO ()
printDependencies _ cf = do
  TIO.putStrLn $ "Dependencies for " <> unPackageName (cfPackageName cf)
  forM_ (cfSections cf) $ \section -> do
    case section of
      LibrarySection lib -> 
        printSection (maybe "Library" ("Library " <>) (libName lib)) (libBuildDepends lib)
      ExecutableSection exe -> 
        printSection ("Executable " <> exeName exe) (exeBuildDepends exe)
      TestSuiteSection test -> 
        printSection ("Test Suite " <> testName test) (testBuildDepends test)
      BenchmarkSection bench -> 
        printSection ("Benchmark " <> benchName bench) (benchBuildDepends bench)
      CommonStanzaSection common ->
        printSection ("Common Stanza " <> commonName common) (commonBuildDepends common)
      _ -> return ()

printSection :: T.Text -> [Dependency] -> IO ()
printSection name deps = do
  if null deps then return () else do
    TIO.putStrLn $ "\n" <> name <> ":"
    forM_ (sort deps) $ \dep -> do
       let ver = formatVersionConstraint (depVersionConstraint dep)
       TIO.putStrLn $ "  - " <> unPackageName (depName dep) <> " " <> ver

formatVersionConstraint :: Maybe VersionConstraint -> T.Text
formatVersionConstraint Nothing = "(*)"
formatVersionConstraint (Just vc) = case vc of
  AnyVersion -> "(*)"
  ExactVersion v -> "==" <> T.pack (show v) -- Simplified
  MajorBoundVersion v -> "^>=" <> T.pack (show v)
  RangeVersion _ -> "(range)" -- TODO: Pretty print internal range
  UnparsedVersion t -> t
  CabalVersionRange vr -> T.pack $ DP.prettyShow vr
  WorkspaceVersion -> "(workspace)"
