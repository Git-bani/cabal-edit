{-# LANGUAGE OverloadedStrings #-}

module Business.Add (addDependency) where

import Core.Types
import Core.Parser
import Core.Serializer
import Core.Safety
import Core.DependencyResolver
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

addDependency :: AddOptions -> FilePath -> IO (Result ())
addDependency opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  -- 1. Validate package name and create typed value
  case mkPackageName (aoPackageName opts) of
    Left err -> return $ Failure $ Error err InvalidDependency
    Right pkgName -> do
      -- 2. Parse cabal file
      parseResult <- parseCabalFile path
      case parseResult of
        Failure err -> return $ Failure err
        Success cabalFile -> do
          
          -- 3. Resolve version / Constraint
          -- Note: resolveVersionConstraint now expects PackageName
          constraintResult <- resolveVersionConstraint pkgName (aoVersion opts)
          
          case constraintResult of
            Failure err -> return $ Failure err
            Success constraint -> do
              -- 4. Determine target section
              let section = findSection (aoSection opts) cabalFile
              
              case section of
                Nothing -> return $ Failure $ Error "Section not found" FileNotFound
                Just sec -> do
                  -- 5. Create dependency
                  let dep = Dependency
                        { depName = pkgName
                        , depVersionConstraint = Just constraint
                        , depType = if aoDev opts then TestDepends else BuildDepends
                        }
                  
                  -- 6. Serialize and Write
                  let TextSpan (TextOffset start) (TextOffset end) = getSectionBounds sec
                  let fullContent = cfRawContent cabalFile
                  let (before, rest) = T.splitAt start fullContent
                  let (sectionContent, after) = T.splitAt (end - start) rest
                  
                  let eol = cfLineEndings cabalFile
                  
                  -- Check if dependency exists to avoid duplicates
                  let existingDeps = findDependencies sec
                  let alreadyExists = any (\d -> depName d == pkgName) existingDeps
                  
                  let newSectionContent = if alreadyExists
                        then updateDependencyLine eol leadingComma dep sectionContent
                        else insertDependencyLine eol leadingComma dep sectionContent
                        
                  let newFullContent = before <> newSectionContent <> after
                  
                  if aoDryRun opts
                    then do
                      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                      TIO.putStrLn newFullContent
                      return $ Success ()
                    else safeWriteCabal path newFullContent
