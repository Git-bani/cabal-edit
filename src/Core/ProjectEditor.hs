{-# LANGUAGE OverloadedStrings #-}

module Core.ProjectEditor
  ( addSourceRepository
  , addLocalPackage
  , ensureProjectFile
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Core.Types (Result(..))
import Core.ProjectContext (findProjectRoot)
import Core.Safety (safeWriteFile) -- Generic safe write logic

-- | Ensure cabal.project exists. If not, create it.
ensureProjectFile :: IO (FilePath, Bool) -- (Path, Created)
ensureProjectFile = do
  maybeRoot <- findProjectRoot
  case maybeRoot of
    Just root -> return (root </> "cabal.project", False)
    Nothing -> do
      cwd <- getCurrentDirectory
      let path = cwd </> "cabal.project"
      exists <- doesFileExist path
      if exists
        then return (path, False)
        else do
          TIO.writeFile path "packages: .\n"
          return (path, True)

-- | Add a source-repository-package block to cabal.project
addSourceRepository :: FilePath -> Text -> Maybe Text -> IO (Result ())
addSourceRepository path url tag = do
  content <- TIO.readFile path
  
  -- Check for duplicates (simple heuristic)
  if url `T.isInfixOf` content
    then return $ Success ()
    else do
      let newBlock = formatSourceRepoBlock url tag
      let newContent = appendBlock content newBlock
      safeWriteFile path newContent

-- | Add a local package path to packages: section
addLocalPackage :: FilePath -> Text -> IO (Result ())
addLocalPackage path localPath = do
  content <- TIO.readFile path
  
  if localPath `T.isInfixOf` content
    then return $ Success ()
    else do
      -- We need to find "packages:" and append, or add "packages:"
      -- For simplicity, we can just add a new "packages:" line at the end
      -- Cabal supports multiple packages: fields (it merges them).
      -- Or we can try to be smarter.
      
      let newField = "\npackages: " <> localPath <> "\n"
      let newContent = content <> newField
      safeWriteFile path newContent

formatSourceRepoBlock :: Text -> Maybe Text -> Text
formatSourceRepoBlock url tag = 
  "\nsource-repository-package\n" <>
  "    type: git\n" <> 
  "    location: " <> url <> "\n" <> 
  maybe "" (\(t) -> "    tag: " <> t <> "\n") tag

appendBlock :: Text -> Text -> Text
appendBlock content block = 
  if T.null content || T.last content == '\n'
  then content <> block
  else content <> "\n" <> block
