{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Core.Types 
  ( -- * Core Types
    CabalFile(..)
  , Section(..)
  , Library(..)
  , Executable(..)
  , TestSuite(..)
  , Benchmark(..)
  , CommonStanza(..)
  , FlagStanza(..)
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
  , FlagOptions(..)
  , FlagOperation(..)
  , ListOptions(..)
  
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
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Strongly typed package name
newtype PackageName = PackageName Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData)

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, NFData, Real, Integral)

-- | Type-safe text span
data TextSpan = TextSpan
  { spanStart :: TextOffset
  , spanEnd   :: TextOffset
  } deriving (Show, Eq, Generic, NFData)

-- | Targeted section for commands
data SectionTarget
  = TargetLib
  | TargetExe (Maybe Text)  -- Nothing = any/first, Just = specific name
  | TargetTest (Maybe Text)
  | TargetBench (Maybe Text)
  | TargetCommon (Maybe Text)
  | TargetNamed Text        -- Specific name, type unknown (legacy/fuzzy match)
  | TargetConditional SectionTarget Text -- Target a specific 'if' block inside a section
  deriving (Show, Eq, Generic, NFData)

-- Represents a parsed Cabal file with structure preservation
data CabalFile = CabalFile
  { cfPackageName :: PackageName
  , cfSections :: [Section]
  , cfRawContent :: Text  -- Original content for format preservation
  , cfLineEndings :: Text -- Detected line ending ("\n" or "\r\n")
  } deriving (Show, Eq, Generic, NFData)

-- Represents a section in a Cabal file
data Section
  = LibrarySection Library
  | ExecutableSection Executable
  | TestSuiteSection TestSuite
  | BenchmarkSection Benchmark
  | CommonStanzaSection CommonStanza
  | FlagSection FlagStanza
  | UnknownSection Text Text  -- name, content
  deriving (Show, Eq, Generic, NFData)

data FlagStanza = FlagStanza
  { flagName :: Text
  , flagDefault :: Bool
  , flagManual :: Bool
  , flagPosition :: TextSpan
  } deriving (Show, Eq, Generic, NFData)

data Library = Library
  { libName :: Maybe Text
  , libBuildDepends :: [Dependency]
  , libPosition :: TextSpan
  } deriving (Show, Eq, Generic, NFData)

data CommonStanza = CommonStanza
  { commonName :: Text
  , commonBuildDepends :: [Dependency]
  , commonPosition :: TextSpan
  } deriving (Show, Eq, Generic, NFData)

data Executable = Executable
  { exeName :: Text
  , exeBuildDepends :: [Dependency]
  , exePosition :: TextSpan
  } deriving (Show, Eq, Generic, NFData)

data TestSuite = TestSuite
  { testName :: Text
  , testBuildDepends :: [Dependency]
  , testPosition :: TextSpan
  } deriving (Show, Eq, Generic, NFData)

data Benchmark = Benchmark
  { benchName :: Text
  , benchBuildDepends :: [Dependency]
  , benchPosition :: TextSpan
  } deriving (Show, Eq, Generic, NFData)

-- Dependency representation
data Dependency = Dependency
  { depName :: PackageName
  , depVersionConstraint :: Maybe VersionConstraint
  , depType :: DependencyType
  } deriving (Show, Eq, Generic, NFData)

instance Ord Dependency where
  compare d1 d2 = compare (depName d1) (depName d2)

data DependencyType = BuildDepends | TestDepends | BenchmarkDepends
  deriving (Show, Eq, Generic, NFData)

-- Version constraint handling
data VersionConstraint
  = AnyVersion
  | ExactVersion Version
  | MajorBoundVersion Version
  | WorkspaceVersion
  | RangeVersion VersionRange
  | UnparsedVersion Text
  | CabalVersionRange VR.VersionRange
  deriving (Show, Eq, Generic, NFData)

data VersionRange = VersionRange
  { lowerBound :: Maybe (Version, BoundType)
  , upperBound :: Maybe (Version, BoundType)
  } deriving (Show, Eq, Generic, NFData)

data BoundType = Inclusive | Exclusive
  deriving (Show, Eq, Generic, NFData)

data Version = Version [Int] 
  deriving (Eq, Ord, Show, Generic, NFData)

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
  | FlagCmd FlagOptions
  | ListCmd ListOptions
  deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ListOptions = ListOptions
  { loJSON :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data FlagOptions = FlagOptions
  { foFlagName :: Maybe Text
  , foOperation :: FlagOperation
  , foDryRun :: Bool
  , foInteractive :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data FlagOperation = FlagAdd | FlagEnable | FlagDisable | FlagRemove
  deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data SetVersionOptions = SetVersionOptions
  { svoVersion :: Text
  , svoDryRun :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data AddOptions = AddOptions
  { aoVersion :: Maybe Text
  , aoSection :: SectionTarget
  , aoCondition :: Maybe Text
  , aoFlag :: Maybe Text
  , aoDev :: Bool
  , aoDryRun :: Bool
  , aoGit :: Maybe Text
  , aoTag :: Maybe Text
  , aoPath :: Maybe Text
  , aoPackageNames :: [Text]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data RemoveOptions = RemoveOptions
  { roSection :: SectionTarget
  , roDryRun :: Bool
  , roInteractive :: Bool
  , roPackageNames :: [Text]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data UpgradeOptions = UpgradeOptions
  { uoDryRun :: Bool
  , uoInteractive :: Bool
  , uoPackageNames :: [Text]  -- Empty means all
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- Result types
data Result a
  = Success a
  | Failure Error
  deriving (Show, Eq, Generic, NFData)

data Error = Error
  { errorMessage :: Text
  , errorCode :: ErrorCode
  } deriving (Show, Eq, Generic, NFData)

instance Exception Error

data ErrorCode
  = ParseError
  | NetworkError
  | FileNotFound
  | InvalidDependency
  | VersionConflict
  | FileModificationError
  | SecurityError
  deriving (Show, Eq, Enum, Generic, NFData)
