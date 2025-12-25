{-# LANGUAGE OverloadedStrings #-}

module Business.Remove (removeDependency) where

import Core.Types
import Core.Parser
import Core.Serializer
import Core.Safety
import Utils.Logging (logInfo)
import Utils.Config (loadConfig, Config(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

removeDependency :: RemoveOptions -> FilePath -> IO (Result ())
removeDependency opts path = do
  cfg <- loadConfig
  let leadingComma = cfgLeadingComma cfg
  case mkPackageName (roPackageName opts) of
    Left err -> return $ Failure $ Error err InvalidDependency
    Right pkgName -> do
      -- 1. Parse cabal file
      parseResult <- parseCabalFile path
      case parseResult of
        Failure err -> return $ Failure err
        Success cabalFile -> do
          -- 2. Determine target section
          let section = findSection (roSection opts) cabalFile
          
          case section of
            Nothing -> return $ Failure $ Error "Section not found" FileNotFound
            Just sec -> do
              -- 3. Check if dependency exists
              let deps = findDependencies sec
              
              if not (any (\d -> depName d == pkgName) deps)
                then return $ Failure $ Error ("Dependency not found: " <> unPackageName pkgName) InvalidDependency
                else do
                  -- 4. Create dummy dependency object for removal (we only need the name)
                  let dep = Dependency
                        { depName = pkgName
                        , depVersionConstraint = Nothing
                        , depType = BuildDepends -- Type doesn't matter for removal
                        }
                  
                  -- 5. Serialize and Write (Scoped to section)
                  let TextSpan (TextOffset start) (TextOffset end) = getSectionBounds sec
                  let fullContent = cfRawContent cabalFile
                  let (before, rest) = T.splitAt start fullContent
                  let (sectionContent, after) = T.splitAt (end - start) rest
                  
                  let eol = cfLineEndings cabalFile
                  let newSectionContent = removeDependencyLine eol leadingComma dep sectionContent
                  let newFullContent = before <> newSectionContent <> after
                  
                  if roDryRun opts
                    then do
                      logInfo $ "Dry run: Proposed changes for " <> T.pack path <> ":"
                      TIO.putStrLn newFullContent
                      return $ Success ()
                    else safeWriteCabal path newFullContent
