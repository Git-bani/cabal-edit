{-# LANGUAGE OverloadedStrings #-}

module Business.Add (addDependency) where

import Core.Types
import Core.Parser
import Core.Serializer
import Core.Safety
import Core.DependencyResolver
import Core.ProjectEditor
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

addDependency :: AddOptions -> FilePath -> IO (Result ())
addDependency opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  
  -- 0. Handle Source Dependencies (Git / Path)
  sourceDepResult <- handleSourceDependency opts
  case sourceDepResult of
    Failure err -> return $ Failure err
    Success () -> do
      -- 1. Validate package name
      case mkPackageName (aoPackageName opts) of
        Left err -> return $ Failure $ Error err InvalidDependency
        Right pkgName -> do
          -- 2. Parse cabal file
          parseResult <- parseCabalFile path
          case parseResult of
            Failure err -> return $ Failure err
            Success cabalFile -> do
              
              -- 3. Resolve version / Constraint
              -- If source dep is used, we default to AnyVersion (no constraint) unless specified
              let isSourceDep = isJust (aoGit opts) || isJust (aoPath opts)
              
              constraintResult <- if isSourceDep && isNothing (aoVersion opts)
                                  then return $ Success AnyVersion
                                  else resolveVersionConstraint pkgName (aoVersion opts)
              
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

handleSourceDependency :: AddOptions -> IO (Result ())
handleSourceDependency opts
  | aoDryRun opts = return $ Success () -- Skip project mod in dry-run for now
  | Just gitUrl <- aoGit opts = do
      (projPath, _) <- ensureProjectFile
      logInfo $ "Adding git dependency to " <> T.pack projPath
      addSourceRepository projPath gitUrl (aoTag opts)
  | Just localPath <- aoPath opts = do
      (projPath, _) <- ensureProjectFile
      logInfo $ "Adding local dependency to " <> T.pack projPath
      addLocalPackage projPath localPath
  | otherwise = return $ Success ()

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False
