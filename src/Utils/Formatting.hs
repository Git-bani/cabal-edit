{-# LANGUAGE OverloadedStrings #-}

module Utils.Formatting
  ( formatInfo
  , formatSuccess
  , formatWarning
  , formatError
  , indentText
  , describeTarget
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Core.Types (SectionTarget(..))
import Data.Maybe (maybeToList)

-- | Format informational message (blue)
formatInfo :: Text -> Text
formatInfo t = "\x1b[34m" <> t <> "\x1b[0m"

-- | Format success message (green)
formatSuccess :: Text -> Text
formatSuccess t = "\x1b[32m" <> t <> "\x1b[0m"

-- | Format warning message (yellow)
formatWarning :: Text -> Text
formatWarning t = "\x1b[33m" <> t <> "\x1b[0m"

-- | Format error message (red)
formatError :: Text -> Text
formatError t = "\x1b[31m" <> t <> "\x1b[0m"

-- | Indent text by N spaces
indentText :: Int -> Text -> Text
indentText n t = 
  let spaces = T.replicate n " "
  in T.unlines $ map (spaces <>) (T.lines t)

-- | Convert SectionTarget to human-readable string
describeTarget :: SectionTarget -> Text
describeTarget TargetLib = "library"
describeTarget (TargetNamed n) = n 
describeTarget (TargetExe mn) = T.unwords $ ["executable"] ++ maybeToList mn
describeTarget (TargetTest mn) = T.unwords $ ["test-suite"] ++ maybeToList mn
describeTarget (TargetBench mn) = T.unwords $ ["benchmark"] ++ maybeToList mn
describeTarget (TargetCommon mn) = T.unwords $ ["common"] ++ maybeToList mn
describeTarget (TargetConditional base _) = describeTarget base