{-# LANGUAGE OverloadedStrings #-}
module Core.AST.Serializer
  ( serializeAST
  , formatDependency
  , formatVersionConstraint
  ) where

import Core.AST.Types
import Core.Types (Dependency(..), VersionConstraint(..), Version(..), VersionRange(..), BoundType(..), unPackageName)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Pretty as CabalPretty

serializeAST :: CabalAST -> Text
serializeAST (CabalAST items) = T.intercalate "\n" (map serializeItem items)

serializeItem :: CabalItem -> Text
serializeItem (EmptyLineItem t) = t
serializeItem (CommentItem t) = t
serializeItem (FieldItem fl) = serializeField fl
serializeItem (SectionItem sl children) = 
  serializeSection sl <> "\n" <> T.intercalate "\n" (map serializeItem children)
serializeItem (IfBlock il thenItems elsePart) = 
  serializeIf il <> "\n" <> 
  T.intercalate "\n" (map serializeItem thenItems) <> 
  maybe "" serializeElse elsePart

serializeField :: FieldLine -> Text
serializeField fl = 
  let indent = T.replicate (fieldIndent fl) " "
  in indent <> fieldName fl <> ":" <> formatFieldValue (fieldIndent fl) (fieldValue fl)

formatFieldValue :: Int -> Text -> Text
formatFieldValue _ val = val

ensureIndent :: Int -> Text -> Text
ensureIndent target t
  | T.null (T.strip t) = t
  | T.length (T.takeWhile (== ' ') t) >= target = t
  | otherwise = T.replicate target " " <> T.stripStart t

serializeSection :: SectionLine -> Text
serializeSection sl = 
  let indent = T.replicate (sectionIndent sl) " "
      args = if T.null (sectionArgs sl) then "" else " " <> sectionArgs sl
  in indent <> sectionType sl <> args

serializeIf :: IfLine -> Text
serializeIf il = 
  let indent = T.replicate (ifIndent il) " "
  in indent <> "if " <> ifCondition il

serializeElse :: (ElseLine, [CabalItem]) -> Text
serializeElse (el, items) = 
  let indent = T.replicate (elseIndent el) " "
  in "\n" <> indent <> "else\n" <> T.intercalate "\n" (map serializeItem items)

--------------------------------------------------------------------------------
-- Dependency Formatting
--------------------------------------------------------------------------------

formatDependency :: Dependency -> Text
formatDependency dep = 
  unPackageName (depName dep) <> formatVersionConstraint (depVersionConstraint dep)

formatVersionConstraint :: Maybe VersionConstraint -> Text
formatVersionConstraint Nothing = ""
formatVersionConstraint (Just AnyVersion) = ""
formatVersionConstraint (Just (ExactVersion ver)) = 
  " ==" <> formatVersion ver
formatVersionConstraint (Just (MajorBoundVersion ver)) = 
  " ^>=" <> formatVersion ver
formatVersionConstraint (Just WorkspaceVersion) = ""
formatVersionConstraint (Just (RangeVersion range)) = 
  " " <> formatVersionRange range
formatVersionConstraint (Just (UnparsedVersion v)) = 
  if T.null v then "" else " " <> v
formatVersionConstraint (Just (CabalVersionRange vr)) = 
  " " <> T.pack (CabalPretty.prettyShow vr)

formatVersion :: Version -> Text
formatVersion (Version parts) = 
  T.intercalate "." (map (T.pack . show) parts)

formatVersionRange :: VersionRange -> Text
formatVersionRange range = 
  let lower = case lowerBound range of
        Nothing -> ""
        Just (ver, Inclusive) -> ">=" <> formatVersion ver
        Just (ver, Exclusive) -> ">" <> formatVersion ver
      upper = case upperBound range of 
        Nothing -> ""
        Just (ver, Inclusive) -> "<=" <> formatVersion ver
        Just (ver, Exclusive) -> "<" <> formatVersion ver
      connector = if not (T.null lower) && not (T.null upper)
                  then " && "
                  else ""
   in lower <> connector <> upper