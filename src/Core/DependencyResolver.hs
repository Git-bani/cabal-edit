{-# LANGUAGE OverloadedStrings #-}

module Core.DependencyResolver
  ( resolveLatestVersion
  , resolveVersionConstraint
  ) where

import Core.Types
import External.Hackage (fetchLatestVersion)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Parsec as CabalParsec
import qualified Distribution.Types.VersionRange as CabalVR

-- | Resolve to the latest version from Hackage
resolveLatestVersion :: PackageName -> IO (Result Version)
resolveLatestVersion pkgName = fetchLatestVersion (unPackageName pkgName)

-- | Resolve version constraint
-- If constraint is provided, valid it (locally).
-- If NO constraint is provided, fetch LATEST from Hackage.
resolveVersionConstraint :: PackageName -> Maybe Text -> IO (Result VersionConstraint)
resolveVersionConstraint pkgName Nothing = do
  res <- resolveLatestVersion pkgName
  case res of
    Success v -> return $ Success (ExactVersion v)
    Failure e -> return $ Failure e

resolveVersionConstraint _ (Just constraint) = do
  -- Validate the constraint syntax using Cabal's parser
  case CabalParsec.simpleParsec (T.unpack constraint) :: Maybe CabalVR.VersionRange of
    Nothing -> return $ Failure $ Error ("Invalid version constraint syntax: " <> constraint) InvalidDependency
    Just _  -> return $ Success (UnparsedVersion constraint)
