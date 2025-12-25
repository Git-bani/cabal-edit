{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Distribution.CabalEdit
-- Description : Manage Cabal dependencies from Haskell
-- Copyright   : (c) Anitha Barns, 2025
-- License     : MIT
-- Maintainer  : anithabarns@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides high-level functions to add, remove, and upgrade
-- dependencies in @.cabal@ files while preserving original formatting and comments.
--
-- The tool aims for parity with Rust's @cargo-edit@.
module Distribution.CabalEdit
  ( -- * High-level Commands
    addDependency
  , removeDependency
  , upgradeDependencies

    -- * Command Options
  , AddOptions(..)
  , RemoveOptions(..)
  , UpgradeOptions(..)

    -- * Results and Errors
  , Result(..)
  , Error(..)
  , ErrorCode(..)

    -- * Data Types
  , SectionTarget(..)
  ) where

import Business.Add (addDependency)
import Business.Remove (removeDependency)
import Business.Upgrade (upgradeDependencies)
import Core.Types
  ( SectionTarget(..)
  , AddOptions(..)
  , RemoveOptions(..)
  , UpgradeOptions(..)
  , Result(..)
  , Error(..)
  , ErrorCode(..)
  )
