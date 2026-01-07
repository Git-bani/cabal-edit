{-# LANGUAGE OverloadedStrings #-}

module Business.Remove (removeDependency) where

import Core.Types
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (removeDependencyFromAST, findDependencyInAST, findDependenciesInAST)
import Core.Safety
import Utils.Logging (logInfo)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM)
import Data.Maybe (maybeToList)

import Utils.Terminal (selectItems)
import Utils.Diff (diffLines, colorizeDiff)
import Data.List (nub)

removeDependency :: RemoveOptions -> FilePath -> IO (Result ())
removeDependency opts path = do
  -- 1. Read and parse into AST
  content <- TIO.readFile path
  let ast = parseAST content
  
  -- 2. Handle interactive mode
  packageNames <- if roInteractive opts
    then do
      let allDeps = findDependenciesInAST ast
      let uniqueNames = nub $ map (\(_, _, d) -> unPackageName (depName d)) allDeps
      if null uniqueNames
        then return []
        else selectItems "Select dependencies to remove:" uniqueNames
    else return (roPackageNames opts)

  if null packageNames && not (null (roPackageNames opts) && not (roInteractive opts))
    then return $ Success () -- Nothing to do
    else do
      -- 3. Process each package
      finalContentResult <- foldM (processPackageRemove opts) (Success content) packageNames
      
      case finalContentResult of
        Failure err -> return $ Failure err
        Success finalContent -> 
          if roDryRun opts
            then do
              logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
              let diffs = diffLines (T.lines content) (T.lines finalContent)
              colorizeDiff diffs
              return $ Success ()
            else safeWriteFile path finalContent

processPackageRemove :: RemoveOptions -> Result Text -> Text -> IO (Result Text)
processPackageRemove _ (Failure err) _ = return $ Failure err
processPackageRemove opts (Success currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Failure $ Error err InvalidDependency
    Right pkgName -> do
      -- Determine which sections to remove from
      let targets = case roSection opts of
            TargetLib -> 
              -- Smart detection using AST: find all sections/blocks that HAVE this dependency
              let ast = parseAST currentContent
                  found = findDependencyInAST (unPackageName pkgName) ast
              in if null found 
                 then [TargetLib] -- Default to library if none found (will fail later)
                 else map (\(sec, mCond) -> case mCond of
                                              Nothing -> TargetNamed sec
                                              Just cond -> TargetConditional (TargetNamed sec) cond) found
            other -> [other]
            
      -- Apply removal to all targets
      let result = foldl (applyRemoval pkgName) (Success currentContent) targets
      
      -- If nothing was removed (content same as before), and we didn't already have a failure, return error
      case result of
        Success newContent | newContent == currentContent ->
          return $ Failure $ Error ("Dependency not found: " <> unPackageName pkgName) InvalidDependency
        other -> return other

applyRemoval :: PackageName -> Result Text -> SectionTarget -> Result Text
applyRemoval _ (Failure err) _ = Failure err
applyRemoval pkgName (Success content) target =
  let (targetName, condition) = case target of
        TargetConditional (TargetNamed n) c -> (n, Just c)
        TargetConditional base c -> (describeTarget base, Just c)
        TargetNamed n -> (n, Nothing)
        other -> (describeTarget other, Nothing)
      ast = parseAST content
  in case removeDependencyFromAST targetName condition (unPackageName pkgName) ast of
       Failure err -> Failure err
       Success updatedAST -> Success $ serializeAST updatedAST

describeTarget :: SectionTarget -> Text
describeTarget TargetLib = "library"
describeTarget (TargetNamed n) = n 
describeTarget (TargetExe mn) = T.unwords $ ["executable"] ++ maybeToList mn
describeTarget (TargetTest mn) = T.unwords $ ["test-suite"] ++ maybeToList mn
describeTarget (TargetBench mn) = T.unwords $ ["benchmark"] ++ maybeToList mn
describeTarget (TargetCommon mn) = T.unwords $ ["common"] ++ maybeToList mn
describeTarget (TargetConditional base _) = describeTarget base