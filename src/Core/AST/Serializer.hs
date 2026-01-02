{-# LANGUAGE OverloadedStrings #-}
module Core.AST.Serializer
  ( serializeAST
  ) where

import Core.AST.Types
import Data.Text (Text)
import qualified Data.Text as T

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
  in indent <> fieldName fl <> ":" <> fieldValue fl 
  -- Note: fieldValue might contain newlines. If we parsed them into one Text, we need to ensure they are formatted right.
  -- My Parser merged them with `\n`. If the original had indentation in hanging lines, it is preserved in `fieldValue`?
  -- Let's check Parser: `map snd hangingLines`. `snd` is the raw line text.
  -- Yes, so `fieldValue` contains the raw text of subsequent lines.
  -- `val` (first line value) also preserves leading spaces? 
  -- In Parser: `val = T.drop 1 valPart`. `valPart` includes the colon.
  -- `val` does NOT strip leading spaces. `T.strip val` was NOT called in Parser (wait, let me check).
  -- Parser: `fLine = FieldLine indent name (T.strip val)` -> STRIPPED!
  -- This is a problem for round-tripping. The space after colon is lost if stripped.
  -- I should NOT strip in Parser if I want perfect roundtrip.

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
