{-# LANGUAGE OverloadedStrings #-}

module Core.Parser
  ( parseCabalFile
  , findDependencies
  , findSection
  , findSectionPosition
  , findConditionalPosition
  , scanCommonStanzas
  , parseCommonDeps
  , getSectionBounds
  , resolveTargetBounds
  , describeSection
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
import Data.Maybe (maybeToList, isNothing, mapMaybe)
import Data.List (find)
import Data.Char (isSpace)

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
         , scanCommonStanzas raw
         , scanFlags raw
         ]

scanCommonStanzas :: Text -> [Section]
scanCommonStanzas raw = 
  let ls = zip [0..] $ T.lines raw
      commonHeaders = filter isCommonHeader ls
  in map (buildCommonStanza raw) commonHeaders
  where
    isCommonHeader :: (Int, Text) -> Bool
    isCommonHeader (_, line) = "common " `T.isPrefixOf` T.stripStart line

    buildCommonStanza :: Text -> (Int, Text) -> Section
    buildCommonStanza content (_, line) = 
      let name = T.strip $ T.drop 7 $ T.stripStart line
          fullName = "common " <> name
          pos = findSectionPosition fullName content
          deps = parseCommonDeps content pos
      in CommonStanzaSection $ CommonStanza
           { commonName = name
           , commonBuildDepends = deps
           , commonPosition = pos
           }

scanFlags :: Text -> [Section]
scanFlags raw = 
  let ls = zip [0..] $ T.lines raw
      flagHeaders = filter isFlagHeader ls
  in map (buildFlagStanza raw) flagHeaders
  where
    isFlagHeader :: (Int, Text) -> Bool
    isFlagHeader (_, line) = "flag " `T.isPrefixOf` T.stripStart line

    buildFlagStanza :: Text -> (Int, Text) -> Section
    buildFlagStanza content (_, line) = 
      let name = T.strip $ T.drop 5 $ T.stripStart line
          fullName = "flag " <> name
          pos = findSectionPosition fullName content
          (def, man) = parseFlagFields content pos
      in FlagSection $ FlagStanza
           { flagName = name
           , flagDefault = def
           , flagManual = man
           , flagPosition = pos
           }

parseFlagFields :: Text -> TextSpan -> (Bool, Bool)
parseFlagFields content (TextSpan (TextOffset start) (TextOffset end)) = 
  let sectionContent = T.take (end - start) (T.drop start content)
      ls = map T.strip $ T.lines sectionContent
      def = maybe False parseBool $ findField "default:" ls
      man = maybe False parseBool $ findField "manual:" ls
  in (def, man)
  where
    findField p lines' = 
      case find (\l -> p `T.isPrefixOf` T.toLower l) lines' of
        Just l -> Just $ T.strip $ T.drop (T.length p) l
        Nothing -> Nothing
    
    parseBool t = 
      let s = T.toLower t
      in s == "true" || s == "yes"

parseCommonDeps :: Text -> TextSpan -> [Dependency]
parseCommonDeps content (TextSpan (TextOffset start) (TextOffset end)) = 
  let sectionContent = T.take (end - start) (T.drop start content)
      ls = T.lines sectionContent
      maybeBuildDepends = findIndexMatches "build-depends:" ls
  in case maybeBuildDepends of
       Nothing -> []
       Just idx -> 
         let (_, rest) = splitAt idx ls
         in case rest of
              [] -> []
              (header:body) ->
                 let (_, val) = T.breakOn ":" header
                     firstLineVal = T.strip $ T.drop 1 val
                     
                     indented = takeWhile isIndented body
                     
                     allLines = if T.null firstLineVal then indented else firstLineVal : indented
                     fullStr = T.intercalate " " (map T.strip allLines)
                     
                     depStrings = T.splitOn "," fullStr
                     deps = mapMaybe parseSimpleDep depStrings
                 in deps

    where
      isIndented t = " " `T.isPrefixOf` t || "\t" `T.isPrefixOf` t
      
      findIndexMatches p lines' = 
        case find (\(_, l) -> p `T.isInfixOf` l) (zip [0..] lines') of
          Just (i, _) -> Just i
          Nothing -> Nothing

      parseSimpleDep :: Text -> Maybe Dependency
      parseSimpleDep t = 
        let s = T.strip t
        in if T.null s then Nothing 
           else 
             let (name, _) = T.break isSpace s
             in Just $ Dependency 
                  { depName = unsafeMkPackageName (T.strip name)
                  , depVersionConstraint = Nothing 
                  , depType = BuildDepends 
                  }

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
versionRangeToConstraint = CabalVersionRange

-- | Resolve bounds for a section or a conditional block
resolveTargetBounds :: SectionTarget -> CabalFile -> Text -> Either Error (Int, Int, Text, Text) -- (Start, End, Prefix, Suffix)
resolveTargetBounds target cabalFile currentContent =
  case target of
    TargetConditional base condition ->
      case findSection base cabalFile of
        Nothing -> Left $ Error "Base section not found" FileNotFound
        Just sec ->
          let secHeader = describeSection sec
              TextSpan (TextOffset sStart) (TextOffset sEnd) = findSectionPosition secHeader currentContent
              secContent = T.take (sEnd - sStart) (T.drop sStart currentContent)
              TextSpan (TextOffset cStart) (TextOffset cEnd) = findConditionalPosition condition secContent
          in if cStart == 0 && cEnd == 0
             then 
               let bIndent = detectBaseIndent' secContent
                   prefixStr = if T.null secContent || T.last secContent == '\n' then "" else "\n"
                   newBlock = prefixStr <> "\n" <> T.replicate bIndent " " <> "if " <> condition <> "\n"
               in Right (sEnd, sEnd, newBlock, "")
             else
               Right (sStart + cStart, sStart + cEnd, "", "")
    
    other ->
      case findSection other cabalFile of
        Nothing -> Left $ Error "Section not found" FileNotFound
        Just sec ->
          let secHeader = describeSection sec
              TextSpan (TextOffset start) (TextOffset end) = findSectionPosition secHeader currentContent
          in Right (start, end, "", "")

-- | Helper to find base indent of a section
detectBaseIndent' :: Text -> Int
detectBaseIndent' content =
  let ls = T.lines content
      fieldLines = filter (T.isInfixOf ":") $ filter (not . T.null . T.strip) ls
  in case fieldLines of
       (l:_) -> T.length $ T.takeWhile (== ' ') l
       [] -> 4

describeSection :: Section -> Text
describeSection (LibrarySection lib) = case libName lib of
  Nothing -> "library"
  Just n -> "library " <> n
describeSection (ExecutableSection exe) = "executable " <> exeName exe
describeSection (TestSuiteSection test) = "test-suite " <> testName test
describeSection (BenchmarkSection bench) = "benchmark " <> benchName bench
describeSection (CommonStanzaSection common) = "common " <> commonName common
describeSection (FlagSection f) = "flag " <> flagName f
describeSection (UnknownSection name _) = name

-- | Find position of an 'if' block within section content
findConditionalPosition :: Text -> Text -> TextSpan
findConditionalPosition condition sectionText =
  let target = "if " <> condition
      targetLower = T.toLower target
      ls = zip [0..] $ T.lines sectionText
      match = find (\(_, l) -> targetLower `T.isInfixOf` T.toLower l) ls
  in case match of
       Nothing -> TextSpan 0 0
       Just (lineIdx, line) ->
         let startOffset = T.length $ T.unlines $ map snd $ take lineIdx ls
             actualStart = if startOffset == 0 then 0 else startOffset + 1
             
             indent = T.length $ T.takeWhile (== ' ') line
             restLines = map snd $ drop (lineIdx + 1) ls
             (body, _) = span (\l -> T.null (T.strip l) || T.length (T.takeWhile (== ' ') l) > indent) restLines
             
             headerLen = T.length line + 1
             bodyLen = sum $ map ((+1) . T.length) body
             
             endOffset = actualStart + headerLen + bodyLen
         in TextSpan (TextOffset actualStart) (TextOffset endOffset)

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

-- | Find the start offset of a section, ignoring comments (line and block)
findSectionStart :: Text -> Text -> Maybe Int
findSectionStart target = go 0 0
  where
    targetLower = T.toLower target
    targetLen = T.length target
    
    go :: Int -> Int -> Text -> Maybe Int
    go offset nesting remaining
      | T.null remaining = Nothing
      | otherwise = 
          let (line, rest) = T.breakOn "\n" remaining
              lineLen = T.length line
              advance = if T.null rest then lineLen else lineLen + 1
              rest' = if T.null rest then "" else T.drop 1 rest
              
              (newNesting, matchInLine) = scanLine nesting line
          in case matchInLine of
               Just relativeOffset -> Just (offset + relativeOffset)
               Nothing -> go (offset + advance) newNesting rest'

    scanLine :: Int -> Text -> (Int, Maybe Int)
    scanLine initialNesting line = 
        let lowerLine = T.toLower line
        in scan 0 initialNesting False line lowerLine

    -- scan :: CurrentIdx -> Nesting -> SeenContent -> OriginalLine -> LowerLine -> (FinalNesting, Maybe MatchOffset)
    scan :: Int -> Int -> Bool -> Text -> Text -> (Int, Maybe Int)
    scan idx nesting seenContent l lowerL
      | T.null l = (nesting, Nothing)
      | nesting > 0 = 
          if "{-" `T.isPrefixOf` l then
             scan (idx + 2) (nesting + 1) seenContent (T.drop 2 l) (T.drop 2 lowerL)
          else if "-}" `T.isPrefixOf` l then
             scan (idx + 2) (nesting - 1) seenContent (T.drop 2 l) (T.drop 2 lowerL)
          else
             scan (idx + 1) nesting seenContent (T.tail l) (T.tail lowerL)
      | otherwise = 
          -- Nesting 0
          if "--" `T.isPrefixOf` l then (nesting, Nothing)
          else if "{-" `T.isPrefixOf` l then
             scan (idx + 2) (nesting + 1) seenContent (T.drop 2 l) (T.drop 2 lowerL)
          else if not seenContent && targetLower `T.isPrefixOf` lowerL && checkBoundary targetLen lowerL then
             (nesting, Just idx)
          else
             let c = T.head l
                 isWhitespace = c == ' ' || c == '\t' || c == '\r'
                 newSeen = seenContent || not isWhitespace
             in scan (idx + 1) nesting newSeen (T.tail l) (T.tail lowerL)

    checkBoundary :: Int -> Text -> Bool
    checkBoundary len lineLower = 
      case T.uncons (T.drop len lineLower) of
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
findDependencies (CommonStanzaSection common) = commonBuildDepends common
findDependencies (FlagSection _) = []
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
matchesSectionTarget (TargetCommon Nothing) (CommonStanzaSection _) = True
matchesSectionTarget (TargetCommon (Just name)) (CommonStanzaSection common) = name == commonName common
matchesSectionTarget (TargetNamed name) (LibrarySection lib) = Just name == libName lib
matchesSectionTarget (TargetNamed name) (ExecutableSection exe) = name == exeName exe
matchesSectionTarget (TargetNamed name) (TestSuiteSection test) = name == testName test
matchesSectionTarget (TargetNamed name) (BenchmarkSection bench) = name == benchName bench
matchesSectionTarget (TargetNamed name) (CommonStanzaSection common) = name == commonName common
matchesSectionTarget _ _ = False

getSectionBounds :: Section -> TextSpan
getSectionBounds (LibrarySection lib) = libPosition lib
getSectionBounds (ExecutableSection exe) = exePosition exe
getSectionBounds (TestSuiteSection test) = testPosition test
getSectionBounds (BenchmarkSection bench) = benchPosition bench
getSectionBounds (CommonStanzaSection common) = commonPosition common
getSectionBounds (FlagSection f) = flagPosition f
getSectionBounds (UnknownSection _ _) = TextSpan 0 0