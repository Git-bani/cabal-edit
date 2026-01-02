{-# LANGUAGE OverloadedStrings #-}
module Core.AST.Types 
  ( CabalAST(..)
  , CabalItem(..)
  , FieldLine(..)
  , SectionLine(..)
  , IfLine(..)
  , ElseLine(..)
  ) where

import Data.Text (Text)

-- | Represents a full Cabal file as a list of top-level items.
newtype CabalAST = CabalAST { unCabalAST :: [CabalItem] }
  deriving (Show, Eq)

-- | A single item in the Cabal file structure.
data CabalItem
  = -- | A field assignment (e.g., "name: my-package")
    -- Preserves formatting: Indent, Name, Separator (:), Value, Comment
    FieldItem FieldLine

  | -- | A section block (e.g., "library", "executable app")
    -- Indent, Type (library), Args (app), Opening Brace?, Items, Closing Brace?
    -- Note: Cabal uses indentation-based blocks usually, but we capture the block content.
    SectionItem SectionLine [CabalItem]

  | -- | A conditional block (if/else)
    -- Indent, Condition, Then-block, Else-block
    IfBlock IfLine [CabalItem] (Maybe (ElseLine, [CabalItem]))

  | -- | A comment line (starts with --)
    CommentItem Text

  | -- | An empty line (whitespace only)
    EmptyLineItem Text
  deriving (Show, Eq)

-- | Details about a Field line
data FieldLine = FieldLine
  { fieldIndent :: Int        -- ^ Number of spaces before field name
  , fieldName :: Text         -- ^ Name of the field (e.g., "build-depends")
  , fieldValue :: Text        -- ^ value of the field
  } deriving (Show, Eq)

-- | Details about a Section header line
data SectionLine = SectionLine
  { sectionIndent :: Int
  , sectionType :: Text       -- ^ e.g., "library", "test-suite"
  , sectionArgs :: Text       -- ^ e.g., "my-test"
  } deriving (Show, Eq)

-- | Details about an 'if' line
data IfLine = IfLine
  { ifIndent :: Int
  , ifCondition :: Text       -- ^ e.g., "os(windows)"
  } deriving (Show, Eq)

-- | Details about an 'else' line
data ElseLine = ElseLine
  { elseIndent :: Int
  } deriving (Show, Eq)
