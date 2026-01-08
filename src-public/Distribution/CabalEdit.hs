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
  , setVersion
  , handleFlag

    -- * Command Options
  , AddOptions(..)
  , RemoveOptions(..)
  , UpgradeOptions(..)
  , SetVersionOptions(..)
  , FlagOptions(..)

    -- * Results and Errors
  , Error(..)
  , ErrorCode(..)

    -- * Data Types
  , SectionTarget(..)
  ) where

import Business.Add (addDependency)
import Business.Remove (removeDependency)
import Business.Upgrade (upgradeDependencies)
import Business.SetVersion (setVersion)
import Business.Flag (handleFlag)
import Core.Types
  ( SectionTarget(..)
  , AddOptions(..)
  , RemoveOptions(..)
  , UpgradeOptions(..)
  , SetVersionOptions(..)
  , FlagOptions(..)
  , Error(..)
  , ErrorCode(..)
  )
