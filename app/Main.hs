{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Options.Applicative hiding (Success, Failure)
import Business.Add
import Business.Remove
import Business.Upgrade
import Business.SetVersion
import Core.Types
import Core.ProjectContext
import Utils.Logging
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, listDirectory)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Data.List (isSuffixOf, isPrefixOf)
import Control.Monad (when, forM_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

main :: IO ()
main = do
  cli <- execParser cliInfo
  startTime <- getCurrentTime
  result <- executeCommand cli
  endTime <- getCurrentTime
  
  let diff = diffUTCTime endTime startTime
  let timeStr = printf "%.2fs" (realToFrac diff :: Double)
  
  case result of
    Success _ -> do
        logSuccess $ T.pack $ "Success! (took " ++ timeStr ++ ")"
        exitSuccess
    Failure err -> do
      logError (errorMessage err)
      exitWith (ExitFailure 1)

-- Command parser
cliInfo :: ParserInfo CLI
cliInfo = info (cliParser <**> helper)
  ( fullDesc
  <> progDesc "Manage Cabal dependencies from command line"
  <> header "cabal-edit - A Cargo-edit equivalent for Haskell" )

cliParser :: Parser CLI
cliParser = CLI
  <$> switch
      ( long "verbose"
      <> short 'v'
      <> help "Enable verbose logging" )
  <*> switch
      ( long "quiet"
      <> short 'q'
      <> help "Suppress all output" )
  <*> switch
      ( long "workspace"
      <> short 'w'
      <> help "Apply to all packages in the workspace (cabal.project)" )
  <*> many (T.pack <$> strOption
      ( long "package"
      <> short 'p'
      <> metavar "PACKAGE"
      <> help "Target specific package in workspace" ))
  <*> commandParser

commandParser :: Parser Command
commandParser = subparser
  ( command "add" (info addParser (progDesc "Add a dependency"))
  <> command "rm" (info removeParser (progDesc "Remove a dependency"))
  <> command "upgrade" (info upgradeParser (progDesc "Upgrade dependencies"))
  <> command "set-version" (info setVersionParser (progDesc "Set package version"))
  )

setVersionParser :: Parser Command
setVersionParser = SetVersionCmd <$>
  ( SetVersionOptions
  <$> (T.pack <$> argument str (metavar "VERSION"))
  <*> switch
      ( long "dry-run"
      <> help "Don't write changes" )
  )

addParser :: Parser Command
addParser = AddCmd <$>
  ( AddOptions
  <$> optional (strOption
      ( long "version"
      <> short 'V'
      <> metavar "VERSION"
      <> help "Specify version constraint" ))
  <*> option sectionTargetReader
      ( long "section"
      <> short 's'
      <> metavar "SECTION"
      <> value TargetLib
      <> help "Target section (library/executable/test)" )
  <*> switch
      ( long "dev"
      <> short 'd'
      <> help "Add as test dependency" )
  <*> switch
      ( long "dry-run"
      <> help "Don't write changes" )
  <*> optional (strOption
      ( long "git"
      <> metavar "URL"
      <> help "Git repository URL" ))
  <*> optional (strOption
      ( long "tag"
      <> metavar "TAG"
      <> help "Git tag/branch/commit" ))
  <*> optional (strOption
      ( long "path"
      <> metavar "PATH"
      <> help "Local path" ))
  <*> some (T.pack <$> argument str (metavar "PACKAGE"))
  )

removeParser :: Parser Command
removeParser = RemoveCmd <$>
  ( RemoveOptions
  <$> option sectionTargetReader
      ( long "section"
      <> short 's'
      <> metavar "SECTION"
      <> value TargetLib
      <> help "Target section (library/executable/test)" )
  <*> switch
      ( long "dry-run"
      <> help "Don't write changes" )
  <*> some (T.pack <$> argument str (metavar "PACKAGE"))
  )

upgradeParser :: Parser Command
upgradeParser = UpgradeCmd <$>
  ( UpgradeOptions
  <$> switch
      ( long "dry-run"
      <> help "Don't write changes" )
  <*> many (T.pack <$> argument str (metavar "PACKAGE"))
  )

sectionTargetReader :: ReadM SectionTarget
sectionTargetReader = eitherReader $ \s -> Right $ parseSectionTarget s

parseSectionTarget :: String -> SectionTarget
parseSectionTarget s = 
  let t = T.pack s
  in if ":if:" `T.isInfixOf` t
     then 
       let (base, condPart) = T.breakOn ":if:" t
           cond = T.drop 4 condPart
           baseTarget = parseSectionTarget (T.unpack base)
       in TargetConditional baseTarget cond
     else case s of
       "library" -> TargetLib
       "lib" -> TargetLib
       "executable" -> TargetExe Nothing
       "exe" -> TargetExe Nothing
       "test-suite" -> TargetTest Nothing
       "test" -> TargetTest Nothing
       "benchmark" -> TargetBench Nothing
       "bench" -> TargetBench Nothing
       "common" -> TargetCommon Nothing
       _ -> if "exe:" `isPrefixOf` s then TargetExe (Just $ T.pack $ drop 4 s)
            else if "test:" `isPrefixOf` s then TargetTest (Just $ T.pack $ drop 5 s)
            else if "bench:" `isPrefixOf` s then TargetBench (Just $ T.pack $ drop 6 s)
            else if "common:" `isPrefixOf` s then TargetCommon (Just $ T.pack $ drop 7 s)
            else TargetNamed (T.pack s)

-- Execute parsed command
executeCommand :: CLI -> IO (Result ())
executeCommand (CLI verbose quiet workspace packages cmd) = do
  if quiet 
    then setLogLevel Quiet
    else when verbose $ setLogLevel Debug
    
  when verbose $ logDebug "Verbose mode enabled"
  
  targetFilesWithCtx <- if workspace || not (null packages)
    then do
      logInfo "Scanning workspace for projects..."
      root <- findProjectRoot
      case root of
        Nothing -> do
          if not (null packages)
            then logError "No cabal.project found, cannot target specific packages!"
            else logError "No cabal.project found!"
          return []
        Just r -> do
          logDebug $ "Found project root: " <> T.pack r
          ctx <- loadProjectContext r
          let allPkgs = pcPackages ctx
          if null packages
            then return $ map (Just ctx, ) (map snd allPkgs)
            else 
              let filtered = filter (\(name, _) -> unPackageName name `elem` packages) allPkgs
                  foundNames = map (unPackageName . fst) filtered
                  missing = filter (`notElem` foundNames) packages
              in do
                forM_ missing $ \m -> logError $ "Package not found in workspace: " <> m
                return $ map (Just ctx, ) (map snd filtered)
    else do
      f <- findCabalFile
      return $ maybe [] (\path -> [(Nothing, path)]) f

  if null targetFilesWithCtx
    then return $ Failure $ Error "No .cabal files found" FileNotFound
    else do
      let count = length targetFilesWithCtx
      if count > 1 
        then logInfo $ "Processing " <> T.pack (show count) <> " package(s)..."
        else return ()
      
      forM_ targetFilesWithCtx $ \(mCtx, path) -> do
        res <- runOn mCtx path cmd
        case res of
          Failure e -> logError $ "Failed on " <> T.pack path <> ": " <> errorMessage e
          Success _ -> return ()
      
      return $ Success ()

runOn :: Maybe ProjectContext -> FilePath -> Command -> IO (Result ())
runOn maybeCtx path cmd = do
  let actionDesc = describeAction cmd
  logInfo $ actionDesc <> " (" <> T.pack path <> ")..."
  
  case cmd of
    AddCmd opts -> addDependency maybeCtx opts path
    RemoveCmd opts -> removeDependency opts path
    UpgradeCmd opts -> upgradeDependencies opts path
    SetVersionCmd opts -> setVersion opts path

describeAction :: Command -> Text
describeAction (AddCmd opts) = "Adding " <> T.intercalate ", " (aoPackageNames opts)
describeAction (RemoveCmd opts) = "Removing " <> T.intercalate ", " (roPackageNames opts)
describeAction (UpgradeCmd opts) = 
  if null (uoPackageNames opts) then "Upgrading all dependencies"
  else "Upgrading " <> T.intercalate ", " (uoPackageNames opts)
describeAction (SetVersionCmd opts) = "Setting version to " <> svoVersion opts

-- Find .cabal file in current directory
findCabalFile :: IO (Maybe FilePath)
findCabalFile = do
  cwd <- getCurrentDirectory
  files <- listDirectory cwd
  let cabalFiles = filter (".cabal" `isSuffixOf`) files
  return $ case cabalFiles of
             (f:_) -> Just f
             [] -> Nothing