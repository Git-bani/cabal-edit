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
