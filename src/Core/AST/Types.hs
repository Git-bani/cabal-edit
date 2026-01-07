{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Core.AST.Types
  ( CabalAST(..)
  , CabalItem(..)
  , FieldLine(..)
  , SectionLine(..)
  , IfLine(..)
  , ElseLine(..)
  )
where

import Data.Text (Text)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Represents a full Cabal file as a list of top-level items.
data CabalAST = CabalAST 
  { unCabalAST :: [CabalItem]
  } deriving (Show, Eq, Generic, NFData)

-- | A single item in the Cabal file structure.
data CabalItem
  = -- | A field assignment (e.g., "name: my-package")
    FieldItem FieldLine

  | -- | A section block (e.g., "library", "executable app")
    SectionItem SectionLine [CabalItem]

  | -- | A conditional block (if/else)
    IfBlock IfLine [CabalItem] (Maybe (ElseLine, [CabalItem]))

  | -- | A comment line (starts with --)
    CommentItem Text Text -- ^ Content, Line Ending

  | -- | An empty line (whitespace only)
    EmptyLineItem Text Text -- ^ Content (spaces), Line Ending
  deriving (Show, Eq, Generic, NFData)

-- | Details about a Field line
data FieldLine = FieldLine
  { fieldIndent :: Int        -- ^ Number of spaces before field name
  , fieldName :: Text         -- ^ Name of the field (e.g., "build-depends")
  , fieldValue :: Text        -- ^ value of the field (might contain internal newlines)
  , fieldLineEnding :: Text   -- ^ Newline after the field header/value
  } deriving (Show, Eq, Generic, NFData)

-- | Details about a Section header line
data SectionLine = SectionLine
  { sectionIndent :: Int
  , sectionType :: Text       -- ^ e.g., "library", "test-suite"
  , sectionArgs :: Text       -- ^ e.g., "my-test"
  , sectionLineEnding :: Text
  } deriving (Show, Eq, Generic, NFData)

-- | Details about an 'if' line
data IfLine = IfLine
  { ifIndent :: Int
  , ifCondition :: Text       -- ^ e.g., "os(windows)"
  , ifLineEnding :: Text
  } deriving (Show, Eq, Generic, NFData)

-- | Details about an 'else' line
data ElseLine = ElseLine
  { elseIndent :: Int
  , elseLineEnding :: Text
  } deriving (Show, Eq, Generic, NFData)
