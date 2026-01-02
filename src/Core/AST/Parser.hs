{-# LANGUAGE OverloadedStrings #-}
module Core.AST.Parser 
  ( parseAST
  ) where

import Core.AST.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | Parse the full text into an AST
parseAST :: Text -> CabalAST
parseAST text = CabalAST $ parseBlock 0 (zip [0..] $ T.splitOn "\n" text)

-- | Parse a block of lines at a given minimum indentation
-- Returns the parsed items and the remaining lines
parseBlock :: Int -> [(Int, Text)] -> [CabalItem]
parseBlock _ [] = []
parseBlock minIndent lines'@((_, l):rest)
  | T.null (T.strip l) = EmptyLineItem l : parseBlock minIndent rest
  | "--" `T.isPrefixOf` (T.stripStart l) = CommentItem l : parseBlock minIndent rest
  | otherwise = 
      let currentIndent = countIndent l
      in if currentIndent < minIndent 
         then [] -- End of block
         else 
           -- Process the item
           let (item, remaining) = parseItem currentIndent lines'
           in item : parseBlock minIndent remaining

-- | Parse a single logical item (Field, Section, or If)
-- It might consume multiple lines (e.g., hanging value, or nested block)
parseItem :: Int -> [(Int, Text)] -> (CabalItem, [(Int, Text)])
parseItem _ [] = error "parseItem called on empty list"
parseItem currentIndent ((lineNum, line):rest) = 
  let trimmed = T.stripStart line
  in if "if " `T.isPrefixOf` trimmed
     then parseIfBlock currentIndent ((lineNum, line):rest)
     else if isSectionHeader trimmed
          then parseSectionBlock currentIndent ((lineNum, line):rest)
          else parseField currentIndent ((lineNum, line):rest)

-- | Detect section header (library, executable, etc.)
isSectionHeader :: Text -> Bool
isSectionHeader t = 
  let headers = ["library", "executable", "test-suite", "benchmark", "common", "flag", "source-repository", "custom-setup"]
      firstWord = fst $ T.break isSpace t
  in firstWord `elem` headers

parseSectionBlock :: Int -> [(Int, Text)] -> (CabalItem, [(Int, Text)])
parseSectionBlock _ [] = error "parseSectionBlock called on empty list"
parseSectionBlock indent ((_, line):rest) = 
  let trimmed = T.stripStart line
      (type', args) = T.break isSpace trimmed
      sLine = SectionLine indent type' (T.strip args)
      
      -- Detect block content
      -- We assume the block content starts at a higher indentation
      -- OR it's a "library" (main) which might not have indentation if at top level?
      -- Actually, main library often has fields indented.
      -- Let's peek at the next non-empty line to guess indentation.
      
      (blockLines, remaining) = takeBlock rest indent
      
      -- If blockLines is not empty, calculate its base indent?
      children = if null blockLines 
                 then [] 
                 else parseBlock (indent + 1) blockLines -- Use strictly greater indent
                 
  in (SectionItem sLine children, remaining)

parseIfBlock :: Int -> [(Int, Text)] -> (CabalItem, [(Int, Text)])
parseIfBlock _ [] = error "parseIfBlock called on empty list"
parseIfBlock indent ((_, line):rest) = 
  let trimmed = T.stripStart line
      cond = T.drop 3 trimmed -- drop "if "
      ifLine = IfLine indent (T.strip cond)
      
      (thenLines, afterThen) = takeBlock rest indent
      thenItems = parseBlock (indent + 1) thenLines
      
      -- Check for else
      (elsePart, finalRemaining) = case afterThen of
        ((_, l):rs) | "else" `T.isPrefixOf` (T.stripStart l) ->
             let elseIndentVal = countIndent l
                 (elseLines, afterElse) = takeBlock rs indent
                 elseItems = parseBlock (indent + 1) elseLines
             in (Just (ElseLine elseIndentVal, elseItems), afterElse)
        _ -> (Nothing, afterThen)
        
  in (IfBlock ifLine thenItems elsePart, finalRemaining)

parseField :: Int -> [(Int, Text)] -> (CabalItem, [(Int, Text)])
parseField _ [] = error "parseField called on empty list"
parseField indent ((_, line):rest) = 
  let (name, valPart) = T.breakOn ":" (T.stripStart line)
      val = T.drop 1 valPart -- drop ":"
      
      fLine = FieldLine indent name val -- Keep raw value for round-trip
      
      -- A field might have "hanging" lines that belong to it
      -- e.g.
      -- build-depends:
      --     base,
      --     text
      --
      -- These lines are indented MORE than the field itself.
      
      (hangingLines, remaining) = span (isHanging indent) rest
      
      -- We need to merge hanging lines into the value?
      -- Or should FieldItem support multiline values naturally?
      -- Current AST defines `fieldValue :: Text`.
      -- Let's assume we just store the initial line value, and separate hanging lines?
      -- No, that breaks "FieldItem".
      --
      -- Wait, my AST definition for FieldItem was: `FieldItem FieldLine`.
      -- `FieldLine` has `fieldValue :: Text`.
      -- If we have multiple lines, we should probably modify `FieldItem` to hold them.
      -- OR, we treat hanging lines as part of the value.
      
      fullValue = if null hangingLines 
                  then val 
                  else val <> "\n" <> T.intercalate "\n" (map snd hangingLines)
                  
      fLine' = fLine { fieldValue = fullValue }
      
  in (FieldItem fLine', remaining)

  where
    isHanging :: Int -> (Int, Text) -> Bool
    isHanging parentIndent (_, l) = 
      let i = countIndent l
      in T.null (T.strip l) || i > parentIndent

takeBlock :: [(Int, Text)] -> Int -> ([(Int, Text)], [(Int, Text)])
takeBlock lines' parentIndent = 
  span (isChild parentIndent) lines'

isChild :: Int -> (Int, Text) -> Bool
isChild parentIndent (_, l) = 
  let i = countIndent l
  in T.null (T.strip l) || i > parentIndent

countIndent :: Text -> Int
countIndent t = T.length $ T.takeWhile (== ' ') t
