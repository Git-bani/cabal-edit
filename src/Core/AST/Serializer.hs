{-# LANGUAGE OverloadedStrings #-}
module Core.AST.Serializer
  ( serializeAST
  , formatDependency
  , formatVersionConstraint
  , formatMixin
  ) where

import Core.AST.Types
import Core.Types (Dependency(..), VersionConstraint(..), Version(..), VersionRange(..), BoundType(..), unPackageName, Mixin(..), Renaming(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Pretty as CabalPretty

serializeAST :: CabalAST -> Text
serializeAST (CabalAST items) = T.intercalate "" (map serializeItem items)

serializeItem :: CabalItem -> Text
serializeItem item = case item of
  EmptyLineItem content term -> content <> term
  CommentItem content term -> content <> term
  FieldItem fl -> serializeField fl
  SectionItem sl children -> 
    let content = serializeSection sl <> T.intercalate "" (map serializeItem children)
        closing = if sectionHasBraces sl 
                  then T.replicate (sectionIndent sl) " " <> "}" <> sectionLineEnding sl
                  else ""
    in content <> closing
  IfBlock il thenItems elsePart -> 
    let content = serializeIf il <> T.intercalate "" (map serializeItem thenItems)
        closing = if ifHasBraces il
                  then case elsePart of
                    Nothing -> T.replicate (ifIndent il) " " <> "}" <> ifLineEnding il
                    Just (el, _) -> 
                      if elseIndent el == 0
                      then T.replicate (ifIndent il) " " <> "} "
                      else T.replicate (ifIndent il) " " <> "}" <> ifLineEnding il
                  else ""
    in content <> closing <> maybe "" (serializeElse (ifIndent il)) elsePart

serializeField :: FieldLine -> Text
serializeField fl = 
  let indent = T.replicate (fieldIndent fl) " "
  in indent <> fieldName fl <> ":" <> fieldValue fl <> fieldLineEnding fl

serializeSection :: SectionLine -> Text
serializeSection sl = 
  let indent = T.replicate (sectionIndent sl) " "
      args = if T.null (sectionArgs sl) then "" else " " <> sectionArgs sl
      braces = if sectionHasBraces sl then " {" else ""
  in indent <> sectionType sl <> args <> braces <> sectionLineEnding sl

serializeIf :: IfLine -> Text
serializeIf il = 
  let indent = T.replicate (ifIndent il) " "
      braces = if ifHasBraces il then " {" else ""
  in indent <> "if " <> ifCondition il <> braces <> ifLineEnding il

serializeElse :: Int -> (ElseLine, [CabalItem]) -> Text
serializeElse parentIfIndent (el, items) = 
  let indent = T.replicate (elseIndent el) " "
      braces = if elseHasBraces el then " {" else ""
      content = T.intercalate "" (map serializeItem items)
      closing = if elseHasBraces el 
                then T.replicate parentIfIndent " " <> "}" <> elseLineEnding el
                else ""
  in indent <> "else" <> braces <> elseLineEnding el <> content <> closing

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

--------------------------------------------------------------------------------
-- Mixin Formatting
--------------------------------------------------------------------------------

formatMixin :: Mixin -> Text
formatMixin (Mixin pkg renaming) =
  unPackageName pkg <> formatRenaming renaming

formatRenaming :: Renaming -> Text
formatRenaming DefaultRenaming = ""
formatRenaming (Hiding mods) =
  if null mods then "" else " hiding (" <> T.intercalate ", " mods <> ")"
formatRenaming (Renaming renames) =
  if null renames then "" else " (" <> T.intercalate ", " (map formatRename renames) <> ")"

formatRename :: (Text, Text) -> Text
formatRename (orig, new) = orig <> " as " <> new
