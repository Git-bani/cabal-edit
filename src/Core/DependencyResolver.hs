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
resolveVersionConstraint :: Maybe ProjectContext -> Maybe Version -> PackageName -> Maybe Text -> VersioningStrategy -> IO (Either Error VersionConstraint)
resolveVersionConstraint maybeCtx mCabalVer pkgName Nothing strategy = do
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
          case strategy of
            StrategyCaret ->
              if supportsCarets mCabalVer
              then return $ Right (MajorBoundVersion v)
              else return $ Right (RangeVersion (computePVPRange v))
            StrategyPVP -> return $ Right (RangeVersion (computePVPRange v))
            StrategyExact -> return $ Right (ExactVersion v)
            StrategyWildcard -> return $ Right (RangeVersion (computeWildcardRange v))
            StrategyNone -> return $ Right AnyVersion
        Left e -> 
          if errorCode e == NetworkError
            then do
              logWarning $ "Could not fetch latest version for " <> unPackageName pkgName <> " (offline?). Adding without constraint."
              return $ Right AnyVersion
            else return $ Left e

resolveVersionConstraint _ _ _ (Just constraint) _ = do
  -- Validate the constraint syntax using Cabal's parser
  case CabalParsec.simpleParsec (T.unpack constraint) :: Maybe CabalVR.VersionRange of
    Nothing -> return $ Left $ Error ("Invalid version constraint syntax: " <> constraint) InvalidDependency
    Just _  -> return $ Right (UnparsedVersion constraint)

-- | Compute wildcard range (e.g. 1.2.*)
computeWildcardRange :: Version -> VersionRange
computeWildcardRange v@(Version parts) =
  let lower = Just (v, Inclusive)
      upper = case reverse parts of
                (_:x:xs) -> Just (Version (reverse (x + 1 : xs)), Exclusive)
                [x]      -> Just (Version [x + 1], Exclusive)
                []       -> Nothing
  in VersionRange lower upper

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
                (a:b:_) -> Just (Version [a, b + 1], Exclusive)
                [a]     -> Just (Version [a, 1], Exclusive)
                []      -> Nothing
  in VersionRange lower upper
