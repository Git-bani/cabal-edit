{-# LANGUAGE OverloadedStrings #-}

module Utils.Error
  ( displayError
  , hintForError
  ) where

import Core.Types
import Data.Text (Text)

-- | Convert an Error into a user-friendly display string
displayError :: Error -> Text
displayError (Error msg code) = 
  case code of
    ParseError -> "Failed to parse file: " <> msg
    NetworkError -> "Network error: " <> msg
    FileNotFound -> "File not found: " <> msg
    InvalidDependency -> "Invalid dependency: " <> msg
    VersionConflict -> "Version conflict: " <> msg
    FileModificationError -> "File error: " <> msg
    SecurityError -> "Security violation: " <> msg

-- | Provide a hint for common errors
hintForError :: Error -> Maybe Text
hintForError (Error _ code) = 
  case code of
    ParseError -> Just "Run 'cabal check' to verify your .cabal file syntax."
    NetworkError -> Just "Check your internet connection or proxy settings."
    FileNotFound -> Just "Ensure you are in the correct directory or the file exists."
    InvalidDependency -> Just "Check the package name spelling on Hackage."
    VersionConflict -> Just "Try relaxing the version constraints."
    FileModificationError -> Just "Check file permissions or if the file is locked."
    _ -> Nothing