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
serializeAST (CabalAST items) = T.intercalate "" (map serializeItem items)

serializeItem :: CabalItem -> Text
serializeItem (EmptyLineItem content term) = content <> term
serializeItem (CommentItem content term) = content <> term
serializeItem (FieldItem fl) = serializeField fl
serializeItem (SectionItem sl children) = 
  serializeSection sl <> T.intercalate "" (map serializeItem children)
serializeItem (IfBlock il thenItems elsePart) = 
  serializeIf il <> 
  T.intercalate "" (map serializeItem thenItems) <> 
  maybe "" serializeElse elsePart

serializeField :: FieldLine -> Text
serializeField fl = 
  let indent = T.replicate (fieldIndent fl) " "
  in indent <> fieldName fl <> ":" <> fieldValue fl <> fieldLineEnding fl

serializeSection :: SectionLine -> Text
serializeSection sl = 
  let indent = T.replicate (sectionIndent sl) " "
      args = if T.null (sectionArgs sl) then "" else " " <> sectionArgs sl
  in indent <> sectionType sl <> args <> sectionLineEnding sl

serializeIf :: IfLine -> Text
serializeIf il = 
  let indent = T.replicate (ifIndent il) " "
  in indent <> "if " <> ifCondition il <> ifLineEnding il

serializeElse :: (ElseLine, [CabalItem]) -> Text
serializeElse (el, items) = 
  let indent = T.replicate (elseIndent el) " "
  in indent <> "else" <> elseLineEnding el <> T.intercalate "" (map serializeItem items)

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
