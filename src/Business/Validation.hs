{-# LANGUAGE OverloadedStrings #-}

module Business.Validation
  ( validatePackageName
  , validateSectionName
  ) where

import Core.Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Validate package name using Core.Types logic
validatePackageName :: Text -> Result ()
validatePackageName name = 
  case mkPackageName name of
    Left err -> Failure $ Error err InvalidDependency
    Right _ -> Success ()

-- | Validate section name
validateSectionName :: Maybe Text -> Result ()
validateSectionName Nothing = Success ()
validateSectionName (Just name)
  | T.null name = 
      Failure $ Error "Section name cannot be empty" InvalidDependency
  | name `elem` validSectionNames = Success ()
  | otherwise = Success () -- Allow flexible names

-- Valid section names in Cabal (Standard types)
validSectionNames :: [Text]
validSectionNames = 
  ["library", "executable", "test-suite", "benchmark", "lib", "exe", "test", "bench"]