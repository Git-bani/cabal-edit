{-# LANGUAGE OverloadedStrings #-}

module Core.HpackEditor
  ( isHpackFile
  , updateHpackDependencies
  ) where

import Core.Types
import Data.Text (Text)
import qualified Data.Text as T

isHpackFile :: FilePath -> Bool
isHpackFile path = "package.yaml" `T.isSuffixOf` T.pack path

-- | Update dependencies in package.yaml
-- This uses a surgical text-based approach to preserve comments and formatting.
updateHpackDependencies :: Text -> [Dependency] -> DependencyOperation -> Text
updateHpackDependencies content deps Add = 
  foldr insertHpackDependency content deps
updateHpackDependencies content deps Remove = 
  foldr removeHpackDependency content deps
updateHpackDependencies content _ Update = 
  content -- TODO: Implement update for Hpack

insertHpackDependency :: Dependency -> Text -> Text
insertHpackDependency dep content =
  let ls = T.lines content
      -- 1. Find the 'dependencies:' block
      -- 2. Find the correct indentation
      -- 3. Insert the new dependency
      (before, target, after) = findDependenciesBlock ls
  in case target of
       Nothing -> 
         -- No dependencies block found, create one at the top level
         content <> "\ndependencies:\n  - " <> formatHpackDep dep <> "\n"
       Just (indent, blockLines) ->
         let newLines = insertIntoSortedHpack (indent + 2) dep blockLines
         in T.unlines (before ++ newLines ++ after)

removeHpackDependency :: Dependency -> Text -> Text
removeHpackDependency dep content =
  let ls = T.lines content
      (before, target, after) = findDependenciesBlock ls
  in case target of
       Nothing -> content
       Just (_, blockLines) ->
         let name = unPackageName (depName dep)
             newLines = filter (not . isTargetDep name) blockLines
         in T.unlines (before ++ newLines ++ after)

isTargetDep :: Text -> Text -> Bool
isTargetDep name line = 
  let trimmed = T.stripStart line
  in if "- " `T.isPrefixOf` trimmed
     then let val = T.drop 2 trimmed
              (depNamePart, _) = T.breakOn " " val
          in T.strip depNamePart == name
     else False

findDependenciesBlock :: [Text] -> ([Text], Maybe (Int, [Text]), [Text])
findDependenciesBlock allLines =
  case break isDepsLine allLines of
    (before, []) -> (before, Nothing, [])
    (before, header:rest) ->
      let indent = T.length $ T.takeWhile (== ' ') header
          (block, after) = span (\l -> T.null (T.strip l) || T.length (T.takeWhile (== ' ') l) > indent) rest
      in (before ++ [header], Just (indent, block), after)
  where
    isDepsLine l = 
      let trimmed = T.strip l
      in trimmed == "dependencies:"

insertIntoSortedHpack :: Int -> Dependency -> [Text] -> [Text]
insertIntoSortedHpack indent dep existing =
  let newEntry = T.replicate indent " " <> "- " <> formatHpackDep dep
      -- For now, just append. Sorting Hpack YAML is more complex due to comments.
      -- But let's try a simple alphabetical insertion if no comments are present.
  in existing ++ [newEntry]

formatHpackDep :: Dependency -> Text
formatHpackDep dep = 
  let name = unPackageName (depName dep)
  in case depVersionConstraint dep of
       Nothing -> name
       Just (UnparsedVersion v) -> name <> ": " <> v
       Just (CabalVersionRange vr) -> name <> ": \"" <> T.pack (show vr) <> "\""
       _ -> name -- Simplified for now
