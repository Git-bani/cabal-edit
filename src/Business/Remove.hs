{-# LANGUAGE OverloadedStrings #-}

module Business.Remove (removeDependency) where

import Core.Types
import Core.AST.Parser (parseAST)
import Core.AST.Serializer (serializeAST)
import Core.AST.Editor (removeDependencyFromAST, findDependencyInAST, findDependenciesInAST)
import Core.Safety
import Core.Solver (verifyChanges)
import Utils.Logging (logInfo)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (foldM)
import Data.Maybe (maybeToList)

import Utils.Terminal (selectItems)
import Utils.Diff (diffLines, colorizeDiff)
import Data.List (nub)

removeDependency :: RemoveOptions -> FilePath -> IO (Either Error ())
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
    then return $ Right () -- Nothing to do
    else do
      -- 3. Process each package
      finalContentResult <- foldM (processPackageRemove opts) (Right content) packageNames
      
      case finalContentResult of
        Left err -> return $ Left err
        Right finalContent -> do
          -- Solver Check
          verificationResult <- if roCheck opts
                                then verifyChanges path finalContent
                                else return $ Right ()
          
          case verificationResult of
            Left err -> return $ Left err
            Right () -> 
              if roDryRun opts
                then do
                  logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                  let diffs = diffLines (T.lines content) (T.lines finalContent)
                  colorizeDiff diffs
                  return $ Right ()
                else safeWriteFile path finalContent

processPackageRemove :: RemoveOptions -> Either Error Text -> Text -> IO (Either Error Text)
processPackageRemove _ (Left err) _ = return $ Left err
processPackageRemove opts (Right currentContent) pkgNameText = do
  case mkPackageName pkgNameText of
    Left err -> return $ Left $ Error err InvalidDependency
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
      let result = foldl (applyRemoval pkgName) (Right currentContent) targets
      
      -- If nothing was removed (content same as before), and we didn't already have a failure, return error
      case result of
        Right newContent | newContent == currentContent ->
          return $ Left $ Error ("Dependency not found: " <> unPackageName pkgName) InvalidDependency
        other -> return other

applyRemoval :: PackageName -> Either Error Text -> SectionTarget -> Either Error Text
applyRemoval _ (Left err) _ = Left err
applyRemoval pkgName (Right content) target =
  let (targetName, condition) = case target of
        TargetConditional (TargetNamed n) c -> (n, Just c)
        TargetConditional base c -> (describeTarget base, Just c)
        TargetNamed n -> (n, Nothing)
        other -> (describeTarget other, Nothing)
      ast = parseAST content
  in case removeDependencyFromAST targetName condition (unPackageName pkgName) ast of
       Left err -> Left err
       Right updatedAST -> Right $ serializeAST updatedAST

describeTarget :: SectionTarget -> Text
describeTarget TargetLib = "library"
describeTarget (TargetNamed n) = n 
describeTarget (TargetExe mn) = T.unwords $ ["executable"] ++ maybeToList mn
describeTarget (TargetTest mn) = T.unwords $ ["test-suite"] ++ maybeToList mn
describeTarget (TargetBench mn) = T.unwords $ ["benchmark"] ++ maybeToList mn
describeTarget (TargetCommon mn) = T.unwords $ ["common"] ++ maybeToList mn
describeTarget (TargetConditional base _) = describeTarget base