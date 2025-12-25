{-# LANGUAGE OverloadedStrings #-}

module Core.Parser
  ( parseCabalFile
  , findDependencies
  , findSection
  , findSectionPosition
  , getSectionBounds
  ) where

import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.GenericPackageDescription as GPD
import qualified Distribution.Types.PackageDescription as PD
import qualified Distribution.Types.PackageId as PID
import qualified Distribution.Types.PackageName as PN
import qualified Distribution.Types.UnqualComponentName as UCN
import qualified Distribution.Types.CondTree as CT
import qualified Distribution.Types.Dependency as CabalDep
import qualified Distribution.Types.VersionRange as VR
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (maybeToList, isNothing)
import Data.List (find)

-- Parse cabal file while preserving original formatting
parseCabalFile :: FilePath -> IO (Result CabalFile)
parseCabalFile path = do
  raw <- BS.readFile path
  let content = decodeUtf8 raw
  let parseRes = Cabal.runParseResult $ Cabal.parseGenericPackageDescription raw
  case parseRes of
    (_warnings, Right gpd) -> 
      return $ Success $ buildCabalFile gpd content
    (_warnings, Left err) -> 
      return $ Failure $ Error (T.pack $ show err) ParseError

buildCabalFile :: GPD.GenericPackageDescription -> Text -> CabalFile
buildCabalFile gpd raw = 
  CabalFile
    {
     cfPackageName = extractPackageName gpd
    , cfSections = extractSections gpd raw
    , cfRawContent = raw
    , cfLineEndings = detectLineEndings raw
    }

detectLineEndings :: Text -> Text
detectLineEndings text =
  if "\r\n" `T.isInfixOf` text then "\r\n" else "\n"

extractPackageName :: GPD.GenericPackageDescription -> PackageName
extractPackageName gpd = 
  unsafeMkPackageName $ T.pack $ PN.unPackageName $ PID.pkgName $ PD.package $ GPD.packageDescription gpd

extractSections :: GPD.GenericPackageDescription -> Text -> [Section]
extractSections gpd raw = 
  concat [ maybeToList (extractLibrary gpd raw) 
         , extractSubLibraries gpd raw
         , extractExecutables gpd raw
         , extractTestSuites gpd raw
         , extractBenchmarks gpd raw
         ]

-- Extract library section
extractLibrary :: GPD.GenericPackageDescription -> Text -> Maybe Section
extractLibrary gpd raw = 
  case GPD.condLibrary gpd of
    Nothing -> Nothing
    Just condTree -> Just $ LibrarySection $ 
      Library
        {
         libName = Nothing
        , libBuildDepends = extractCondTreeDeps condTree
        , libPosition = findSectionPosition "library" raw
        }

-- Extract Sub-libraries
extractSubLibraries :: GPD.GenericPackageDescription -> Text -> [Section]
extractSubLibraries gpd raw = 
  let subLibs = GPD.condSubLibraries gpd
  in map (convertSubLibrary raw) subLibs

convertSubLibrary :: Text -> (UCN.UnqualComponentName, CT.CondTree v [CabalDep.Dependency] a) -> Section
convertSubLibrary raw (name, condTree) = 
  let tName = T.pack $ UCN.unUnqualComponentName name
  in LibrarySection $ Library
       {
        libName = Just tName
       , libBuildDepends = extractCondTreeDeps condTree
       , libPosition = findSectionPosition ("library " <> tName) raw
       }

-- Extract Executables
extractExecutables :: GPD.GenericPackageDescription -> Text -> [Section]
extractExecutables gpd raw = 
  let exes = GPD.condExecutables gpd
  in map (convertExecutable raw) exes

convertExecutable :: Text -> (UCN.UnqualComponentName, CT.CondTree v [CabalDep.Dependency] a) -> Section
convertExecutable raw (name, condTree) = 
  let tName = T.pack $ UCN.unUnqualComponentName name
  in ExecutableSection $ Executable
       {
        exeName = tName
       , exeBuildDepends = extractCondTreeDeps condTree
       , exePosition = findSectionPosition ("executable " <> tName) raw
       }

-- Extract Test Suites
extractTestSuites :: GPD.GenericPackageDescription -> Text -> [Section]
extractTestSuites gpd raw = 
  let tests = GPD.condTestSuites gpd
  in map (convertTestSuite raw) tests

convertTestSuite :: Text -> (UCN.UnqualComponentName, CT.CondTree v [CabalDep.Dependency] a) -> Section
convertTestSuite raw (name, condTree) = 
  let tName = T.pack $ UCN.unUnqualComponentName name
  in TestSuiteSection $ TestSuite
       {
        testName = tName
       , testBuildDepends = extractCondTreeDeps condTree
       , testPosition = findSectionPosition ("test-suite " <> tName) raw
       }

-- Extract Benchmarks
extractBenchmarks :: GPD.GenericPackageDescription -> Text -> [Section]
extractBenchmarks gpd raw = 
  let benches = GPD.condBenchmarks gpd
  in map (convertBenchmark raw) benches

convertBenchmark :: Text -> (UCN.UnqualComponentName, CT.CondTree v [CabalDep.Dependency] a) -> Section
convertBenchmark raw (name, condTree) = 
  let tName = T.pack $ UCN.unUnqualComponentName name
  in BenchmarkSection $ Benchmark
       {
        benchName = tName
       , benchBuildDepends = extractCondTreeDeps condTree
       , benchPosition = findSectionPosition ("benchmark " <> tName) raw
       }

-- Helper to extract dependencies from a Conditional Tree
-- Note: This only gets top-level dependencies, not those in 'if' blocks
extractCondTreeDeps :: CT.CondTree v [CabalDep.Dependency] a -> [Dependency]
extractCondTreeDeps tree = 
  map cabalDepToInternal $ CT.condTreeConstraints tree

cabalDepToInternal :: CabalDep.Dependency -> Dependency
cabalDepToInternal (CabalDep.Dependency pkgName versionRange _) = 
  Dependency
    {
     depName = unsafeMkPackageName $ T.pack $ PN.unPackageName pkgName
    , depVersionConstraint = Just $ versionRangeToConstraint versionRange
    , depType = BuildDepends
    }

versionRangeToConstraint :: VR.VersionRange -> VersionConstraint
versionRangeToConstraint vr = CabalVersionRange vr

-- Find position of section in raw text
-- Returns TextSpan
findSectionPosition :: Text -> Text -> TextSpan
findSectionPosition sectionName raw = 
  case findSectionStart sectionName raw of
       Nothing -> TextSpan 0 0
       Just start -> 
         let (_, after) = T.splitAt start raw
             (headerLine, rest) = T.breakOn "\n" after
             actualRest = if T.null rest then "" else T.drop 1 rest
             endOffset = start + T.length headerLine + 1 + findSectionEnd actualRest
         in TextSpan (TextOffset start) (TextOffset endOffset)

-- | Find the start offset of a section, ignoring comments
findSectionStart :: Text -> Text -> Maybe Int
findSectionStart target raw = go 0 raw
  where
    targetLower = T.toLower target
    
    go :: Int -> Text -> Maybe Int
    go offset remaining
      | T.null remaining = Nothing
      | otherwise = 
          let (line, rest) = T.breakOn "\n" remaining
              lineLen = T.length line
              advance = if T.null rest then lineLen else lineLen + 1
              rest' = if T.null rest then "" else T.drop 1 rest
              
              trimmed = T.stripStart line
              trimmedLower = T.toLower trimmed
          in 
            if "--" `T.isPrefixOf` trimmed
            then go (offset + advance) rest' 
            else if targetLower `T.isPrefixOf` trimmedLower && checkBoundary (T.length target) trimmedLower
            then Just offset 
            else go (offset + advance) rest'

    checkBoundary :: Int -> Text -> Bool
    checkBoundary targetLen lineLower = 
      case T.uncons (T.drop targetLen lineLower) of
        Nothing -> True
        Just (c, _) -> c `elem` [' ', '\t', '\r']

findSectionEnd :: Text -> Int
findSectionEnd text = 
  let ls = T.lines text
      shouldStop :: Text -> Bool
      shouldStop line = 
        let trimmed = T.strip line
        in not (T.null trimmed) && 
           not (" " `T.isPrefixOf` line) && 
           not ("\t" `T.isPrefixOf` line) &&
           not ("--" `T.isPrefixOf` trimmed)

      (bodyLines, _) = break shouldStop ls
      
      bodyLen = sum $ map ((+1) . T.length) bodyLines
  in bodyLen

findDependencies :: Section -> [Dependency]
findDependencies (LibrarySection lib) = libBuildDepends lib
findDependencies (ExecutableSection exe) = exeBuildDepends exe
findDependencies (TestSuiteSection test) = testBuildDepends test
findDependencies (BenchmarkSection bench) = benchBuildDepends bench
findDependencies (UnknownSection _ _) = []

findSection :: SectionTarget -> CabalFile -> Maybe Section
findSection target cabalFile = 
  find (matchesSectionTarget target) (cfSections cabalFile)

matchesSectionTarget :: SectionTarget -> Section -> Bool
matchesSectionTarget TargetLib (LibrarySection lib) = isNothing (libName lib)
matchesSectionTarget (TargetExe Nothing) (ExecutableSection _) = True
matchesSectionTarget (TargetExe (Just name)) (ExecutableSection exe) = name == exeName exe
matchesSectionTarget (TargetTest Nothing) (TestSuiteSection _) = True
matchesSectionTarget (TargetTest (Just name)) (TestSuiteSection test) = name == testName test
matchesSectionTarget (TargetBench Nothing) (BenchmarkSection _) = True
matchesSectionTarget (TargetBench (Just name)) (BenchmarkSection bench) = name == benchName bench
matchesSectionTarget (TargetNamed name) (LibrarySection lib) = Just name == libName lib
matchesSectionTarget (TargetNamed name) (ExecutableSection exe) = name == exeName exe
matchesSectionTarget (TargetNamed name) (TestSuiteSection test) = name == testName test
matchesSectionTarget (TargetNamed name) (BenchmarkSection bench) = name == benchName bench
matchesSectionTarget _ _ = False

getSectionBounds :: Section -> TextSpan
getSectionBounds (LibrarySection lib) = libPosition lib
getSectionBounds (ExecutableSection exe) = exePosition exe
getSectionBounds (TestSuiteSection test) = testPosition test
getSectionBounds (BenchmarkSection bench) = benchPosition bench
getSectionBounds (UnknownSection _ _) = TextSpan 0 0
