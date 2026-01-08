{-# LANGUAGE OverloadedStrings #-}

module Core.DependencyResolver
  ( resolveLatestVersion
  , resolveVersionConstraint
  ) where

import Core.Types
import Core.ProjectContext (ProjectContext(..))
import External.Hackage (fetchLatestVersion)
import Utils.Logging (logWarning)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Parsec as CabalParsec
import qualified Distribution.Types.VersionRange as CabalVR

-- | Resolve to the latest version from Hackage
resolveLatestVersion :: PackageName -> IO (Either Error Version)
resolveLatestVersion pkgName = fetchLatestVersion (unPackageName pkgName)

-- | Resolve version constraint
-- If constraint is provided, valid it (locally).
-- If NO constraint is provided:
--   1. Check if it's a workspace member (if context provided).
--   2. Otherwise, fetch LATEST from Hackage.
resolveVersionConstraint :: Maybe ProjectContext -> Maybe Version -> PackageName -> Maybe Text -> IO (Either Error VersionConstraint)
resolveVersionConstraint maybeCtx mCabalVer pkgName Nothing = do
  -- 1. Check Workspace
  let isWorkspaceMember = case maybeCtx of
        Nothing -> False
        Just ctx -> any (\(name, _) -> name == pkgName) (pcPackages ctx)
  
  if isWorkspaceMember
    then return $ Right WorkspaceVersion
    else do
      -- 2. Check Hackage
      res <- resolveLatestVersion pkgName
      case res of
        Right v -> 
          if supportsCarets mCabalVer
          then return $ Right (MajorBoundVersion v)
          else return $ Right (RangeVersion (computePVPRange v))
        Left e -> 
          if errorCode e == NetworkError
            then do
              logWarning $ "Could not fetch latest version for " <> unPackageName pkgName <> " (offline?). Adding without constraint."
              return $ Right AnyVersion
            else return $ Left e

resolveVersionConstraint _ _ _ (Just constraint) = do
  -- Validate the constraint syntax using Cabal's parser
  case CabalParsec.simpleParsec (T.unpack constraint) :: Maybe CabalVR.VersionRange of
    Nothing -> return $ Left $ Error ("Invalid version constraint syntax: " <> constraint) InvalidDependency
    Just _  -> return $ Right (UnparsedVersion constraint)

-- | Check if cabal-version supports caret operator (^>=)
-- Supported in Cabal >= 2.0
supportsCarets :: Maybe Version -> Bool
supportsCarets Nothing = False -- Assume legacy if missing
supportsCarets (Just (Version (major:_))) = major >= 2
supportsCarets (Just (Version [])) = False

-- | Compute PVP-compliant range without carets
-- 0.x.y -> >= 0.x.y && < 0.(x+1)
-- 1.x.y -> >= 1.x.y && < 2.0
computePVPRange :: Version -> VersionRange
computePVPRange v@(Version parts) =
  let lower = Just (v, Inclusive)
      upper = case parts of
                (0:y:_) -> Just (Version [0, y + 1], Exclusive)
                (x:_) -> Just (Version [x + 1, 0], Exclusive)
                [] -> Nothing
  in VersionRange lower upper
