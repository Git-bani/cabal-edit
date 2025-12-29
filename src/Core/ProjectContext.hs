{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Core.ProjectContext
  ( ProjectContext(..)
  , findProjectRoot
  , loadProjectContext
  , findAllPackageFiles
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory, takeFileName)
import Control.Monad (filterM)
import Data.List (isSuffixOf, find)
import Core.Types (PackageName, mkPackageName)

data ProjectContext = ProjectContext
  { pcRoot :: FilePath
  , pcPackageGlobs :: [String]
  , pcPackages :: [(PackageName, FilePath)] -- (Name, Path to .cabal file)
  } deriving (Show, Eq)

-- | Find the directory containing cabal.project, traversing upwards
findProjectRoot :: IO (Maybe FilePath)
findProjectRoot = do
  cwd <- getCurrentDirectory
  go cwd
  where
    go :: FilePath -> IO (Maybe FilePath)
    go dir = do
      let projFile = dir </> "cabal.project"
      exists <- doesFileExist projFile
      if exists
        then return $ Just dir
        else do
          let parent = takeDirectory dir
          if parent == dir
            then return Nothing -- Root reached
            else go parent

-- | Load context from project root
loadProjectContext :: FilePath -> IO ProjectContext
loadProjectContext root = do
  let projFile = root </> "cabal.project"
  content <- TIO.readFile projFile
  let globs = parseProjectGlobs content
  let ctxNoPkgs = ProjectContext root globs []
  paths <- findAllPackageFiles ctxNoPkgs
  pkgs <- mapM extractPackageNameSimple paths
  return $ ProjectContext root globs [ (name, p) | (p, Right name) <- zip paths pkgs ]

extractPackageNameSimple :: FilePath -> IO (Either Text PackageName)
extractPackageNameSimple path = do
  content <- TIO.readFile path
  let ls = T.lines content
      nameLine = find (\l -> "name:" `T.isPrefixOf` T.toLower (T.stripStart l)) ls
  case nameLine of
    Just l -> 
      let (_, val) = T.breakOn ":" l
      in return $ mkPackageName $ T.strip $ T.drop 1 val
    Nothing -> 
      -- Fallback to filename
      return $ mkPackageName $ T.pack $ takeFileName path


-- | Robust parser for packages and optional-packages
parseProjectGlobs :: Text -> [String]
parseProjectGlobs content = 
  let projLines = map stripComment $ T.lines content
      -- We assume structure:
      -- field: value
      --   value
      -- field: value
  in extractGlobs projLines
  where
    stripComment :: Text -> Text
    stripComment t = fst $ T.breakOn "--" t

    extractGlobs :: [Text] -> [String]
    extractGlobs [] = []
    extractGlobs (l:ls) = 
      let trimmed = T.strip l
      in if "packages:" `T.isPrefixOf` trimmed || "optional-packages:" `T.isPrefixOf` trimmed
         then 
           let (_, rest) = T.breakOn ":" trimmed
               firstLineVal = T.strip $ T.drop 1 rest
               (bodyLines, remaining) = span isIndentedOrBlank ls
               
               -- Values from the first line
               firstParts = if T.null firstLineVal then [] else T.words firstLineVal
               
               -- Values from subsequent indented lines
               bodyParts = concatMap (T.words . T.strip) bodyLines
               
               currentGlobs = map T.unpack (firstParts ++ bodyParts)
           in currentGlobs ++ extractGlobs remaining
         else extractGlobs ls

    isIndentedOrBlank :: Text -> Bool
    isIndentedOrBlank t = 
      let s = T.strip t
      in T.null s || " " `T.isPrefixOf` t || "\t" `T.isPrefixOf` t

-- | Find all .cabal files based on context
findAllPackageFiles :: ProjectContext -> IO [FilePath]
findAllPackageFiles ctx = do
  -- Default to "." if no globs found (standard Cabal behavior)
  let globs = if null (pcPackageGlobs ctx) then ["."] else pcPackageGlobs ctx
  paths <- mapM (expandGlob (pcRoot ctx)) globs
  return $ concat paths

-- | Expand simple globs
expandGlob :: FilePath -> String -> IO [FilePath]
expandGlob root glob = do
  -- Case 1: Wildcard "dir/*"
  if "*" `isSuffixOf` glob
    then do
      -- Remove the "*" and trailing slash
      let rawBase = takeDirectory glob
      let baseDir = if rawBase == "." then root else root </> rawBase
      
      exists <- doesDirectoryExist baseDir
      if exists
        then do
          entries <- listDirectory baseDir
          -- For each entry, look for .cabal file inside
          -- We only look into subdirectories
          subPaths <- filterM doesDirectoryExist (map (baseDir </>) entries)
          concat <$> mapM findCabalFileInDir subPaths
        else return []
    else do
      -- Case 2: Explicit path "dir/foo" or "."
      let target = root </> glob
      isDir <- doesDirectoryExist target
      if isDir
        then findCabalFileInDir target
        else return []

findCabalFileInDir :: FilePath -> IO [FilePath]
findCabalFileInDir dir = do
  entries <- listDirectory dir
  let cabalFiles = filter (\f -> ".cabal" `isSuffixOf` f) entries
  return $ map (dir </>) cabalFiles
