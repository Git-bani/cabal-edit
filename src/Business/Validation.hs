{-# LANGUAGE OverloadedStrings #-}

module Business.Validation
  ( validatePackageName
  , validateSectionName
  ) where

import Core.Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Validate package name using Core.Types logic
validatePackageName :: Text -> Either Error ()
validatePackageName name = 
  case mkPackageName name of
    Left err -> Left $ Error err InvalidDependency
    Right _ -> Right ()

-- | Validate section name
validateSectionName :: Maybe Text -> Either Error ()
validateSectionName Nothing = Right ()
validateSectionName (Just name)
  | T.null name = 
      Left $ Error "Section name cannot be empty" InvalidDependency
  | name `elem` validSectionNames = Right ()
  | otherwise = Right () -- Allow flexible names

-- Valid section names in Cabal (Standard types)
validSectionNames :: [Text]
validSectionNames = 
  ["library", "executable", "test-suite", "benchmark", "lib", "exe", "test", "bench"]