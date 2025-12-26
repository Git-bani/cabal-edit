{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Types 
  ( -- * Core Types
    CabalFile(..)
  , Section(..)
  , Library(..)
  , Executable(..)
  , TestSuite(..)
  , Benchmark(..)
  , CommonStanza(..)
  , Dependency(..)
  , DependencyType(..)
  , VersionConstraint(..)
  , VersionRange(..)
  , BoundType(..)
  , Version(..)
  , TextOffset(..)
  , TextSpan(..)
  , SectionTarget(..)
  
    -- * Package Name
  , PackageName
  , mkPackageName
  , unPackageName
  , unsafeMkPackageName
  
    -- * CLI / Commands
  , CLI(..)
  , Command(..)
  , AddOptions(..)
  , RemoveOptions(..)
  , UpgradeOptions(..)
  , SetVersionOptions(..)
  
    -- * Results
  , Result(..)
  , Error(..)
  , ErrorCode(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import Control.Exception (Exception)
import qualified Distribution.Types.VersionRange as VR

-- | Strongly typed package name
newtype PackageName = PackageName Text
  deriving (Eq, Ord, Show)

-- | Smart constructor for PackageName
mkPackageName :: Text -> Either Text PackageName
mkPackageName name
  | T.null name = Left "Package name cannot be empty"
  | not (isValidPackageName name) = 
      Left $ "Invalid package name format: '" <> name <> "'. Must contain only letters, numbers, and hyphens."
  | otherwise = Right (PackageName name)

-- | Unsafe constructor for internal use (e.g. Parser)
unsafeMkPackageName :: Text -> PackageName
unsafeMkPackageName = PackageName

unPackageName :: PackageName -> Text
unPackageName (PackageName t) = t

isValidPackageName :: Text -> Bool
isValidPackageName name = 
  let chars = T.unpack name
      validChars = all (\c -> isAlphaNum c || c == '-') chars
      noLeadingTrailingHyphen = 
        not (T.isPrefixOf "-" name) && not (T.isSuffixOf "-" name)
      noConsecutiveHyphens = not $ T.isInfixOf "--" name
  in not (T.null name) && validChars && noLeadingTrailingHyphen && noConsecutiveHyphens

-- | Type-safe text offset (code point index)
newtype TextOffset = TextOffset Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Type-safe text span
data TextSpan = TextSpan
  { spanStart :: TextOffset
  , spanEnd   :: TextOffset
  } deriving (Show, Eq)

-- | Targeted section for commands
data SectionTarget
  = TargetLib
  | TargetExe (Maybe Text)  -- Nothing = any/first, Just = specific name
  | TargetTest (Maybe Text)
  | TargetBench (Maybe Text)
  | TargetCommon (Maybe Text)
  | TargetNamed Text        -- Specific name, type unknown (legacy/fuzzy match)
  | TargetConditional SectionTarget Text -- Target a specific 'if' block inside a section
  deriving (Show, Eq)

-- Represents a parsed Cabal file with structure preservation
data CabalFile = CabalFile
  { cfPackageName :: PackageName
  , cfSections :: [Section]
  , cfRawContent :: Text  -- Original content for format preservation
  , cfLineEndings :: Text -- Detected line ending ("\n" or "\r\n")
  } deriving (Show, Eq)

-- Represents a section in a Cabal file
data Section
  = LibrarySection Library
  | ExecutableSection Executable
  | TestSuiteSection TestSuite
  | BenchmarkSection Benchmark
  | CommonStanzaSection CommonStanza
  | UnknownSection Text Text  -- name, content
  deriving (Show, Eq)

data Library = Library
  { libName :: Maybe Text
  , libBuildDepends :: [Dependency]
  , libPosition :: TextSpan
  } deriving (Show, Eq)

data CommonStanza = CommonStanza
  { commonName :: Text
  , commonBuildDepends :: [Dependency]
  , commonPosition :: TextSpan
  } deriving (Show, Eq)

data Executable = Executable
  { exeName :: Text
  , exeBuildDepends :: [Dependency]
  , exePosition :: TextSpan
  } deriving (Show, Eq)

data TestSuite = TestSuite
  { testName :: Text
  , testBuildDepends :: [Dependency]
  , testPosition :: TextSpan
  } deriving (Show, Eq)

data Benchmark = Benchmark
  { benchName :: Text
  , benchBuildDepends :: [Dependency]
  , benchPosition :: TextSpan
  } deriving (Show, Eq)

-- Dependency representation
data Dependency = Dependency
  { depName :: PackageName
  , depVersionConstraint :: Maybe VersionConstraint
  , depType :: DependencyType
  } deriving (Show, Eq)

instance Ord Dependency where
  compare d1 d2 = compare (depName d1) (depName d2)

data DependencyType = BuildDepends | TestDepends | BenchmarkDepends
  deriving (Show, Eq)

-- Version constraint handling
data VersionConstraint
  = AnyVersion
  | ExactVersion Version
  | MajorBoundVersion Version
  | WorkspaceVersion
  | RangeVersion VersionRange
  | UnparsedVersion Text
  | CabalVersionRange VR.VersionRange
  deriving (Show, Eq)

data VersionRange = VersionRange
  { lowerBound :: Maybe (Version, BoundType)
  , upperBound :: Maybe (Version, BoundType)
  } deriving (Show, Eq)

data BoundType = Inclusive | Exclusive
  deriving (Show, Eq)

data Version = Version [Int] 
  deriving (Eq, Ord, Show)

-- Command types
data CLI = CLI
  { cliVerbose :: Bool
  , cliQuiet :: Bool
  , cliWorkspace :: Bool
  , cliPackages :: [Text] -- Targeted packages in workspace
  , cliCommand :: Command
  } deriving (Show, Eq)

data Command
  = AddCmd AddOptions
  | RemoveCmd RemoveOptions
  | UpgradeCmd UpgradeOptions
  | SetVersionCmd SetVersionOptions
  deriving (Show, Eq)

data SetVersionOptions = SetVersionOptions
  { svoVersion :: Text
  , svoDryRun :: Bool
  } deriving (Show, Eq)

data AddOptions = AddOptions
  { aoVersion :: Maybe Text
  , aoSection :: SectionTarget
  , aoDev :: Bool
  , aoDryRun :: Bool
  , aoGit :: Maybe Text
  , aoTag :: Maybe Text
  , aoPath :: Maybe Text
  , aoPackageNames :: [Text]
  } deriving (Show, Eq)

data RemoveOptions = RemoveOptions
  { roSection :: SectionTarget
  , roDryRun :: Bool
  , roPackageNames :: [Text]
  } deriving (Show, Eq)

data UpgradeOptions = UpgradeOptions
  { uoDryRun :: Bool
  , uoPackageNames :: [Text]  -- Empty means all
  } deriving (Show, Eq)

-- Result types
data Result a
  = Success a
  | Failure Error
  deriving (Show, Eq)

data Error = Error
  { errorMessage :: Text
  , errorCode :: ErrorCode
  } deriving (Show, Eq)

instance Exception Error

data ErrorCode
  = ParseError
  | NetworkError
  | FileNotFound
  | InvalidDependency
  | VersionConflict
  | FileModificationError
  | SecurityError
  deriving (Show, Eq, Enum)
