{-# LANGUAGE OverloadedStrings #-}

module Core.DependencyResolver
  ( resolveLatestVersion
  , resolveVersionConstraint
  ) where

import Core.Types
import Core.ProjectContext (ProjectContext(..))
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
-- If NO constraint is provided:
--   1. Check if it's a workspace member (if context provided).
--   2. Otherwise, fetch LATEST from Hackage.
resolveVersionConstraint :: Maybe ProjectContext -> PackageName -> Maybe Text -> IO (Result VersionConstraint)
resolveVersionConstraint maybeCtx pkgName Nothing = do
  -- 1. Check Workspace
  let isWorkspaceMember = case maybeCtx of
        Nothing -> False
        Just ctx -> any (\(name, _) -> name == pkgName) (pcPackages ctx)
  
  if isWorkspaceMember
    then return $ Success WorkspaceVersion
    else do
      -- 2. Check Hackage
      res <- resolveLatestVersion pkgName
      case res of
        Success v -> return $ Success (MajorBoundVersion v)
        Failure e -> return $ Failure e

resolveVersionConstraint _ _ (Just constraint) = do
  -- Validate the constraint syntax using Cabal's parser
  case CabalParsec.simpleParsec (T.unpack constraint) :: Maybe CabalVR.VersionRange of
    Nothing -> return $ Failure $ Error ("Invalid version constraint syntax: " <> constraint) InvalidDependency
    Just _  -> return $ Success (UnparsedVersion constraint)
