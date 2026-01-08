{-# LANGUAGE OverloadedStrings #-}
module Core.AST.Parser
 
  ( parseAST
  ) where

import Core.AST.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | Split text into lines, preserving the terminator for each line.
-- Returns [(LineContent, Terminator)]
splitLinesPreserving :: Text -> [(Text, Text)]
splitLinesPreserving t
  | T.null t = []
  | otherwise = 
      let (line, rest) = T.break (`elem` ['\n', '\r']) t
          (term, rest') = if "\r\n" `T.isPrefixOf` rest
                          then ("\r\n", T.drop 2 rest)
                          else if "\n" `T.isPrefixOf` rest
                               then ("\n", T.drop 1 rest)
                               else ("", "") -- EOF without newline
          next = if T.null rest' then [] else splitLinesPreserving rest'
      in (line, term) : next

-- | Parse the full text into an AST.
-- Currently still returns CabalAST directly for compatibility, 
-- but internal functions are now total.
parseAST :: Text -> CabalAST
parseAST text = CabalAST $ parseBlock 0 (splitLinesPreserving text)

-- | Parse a block of lines at a given minimum indentation
parseBlock :: Int -> [(Text, Text)] -> [CabalItem]
parseBlock _ [] = []
parseBlock minIndent lines'@((l, term):rest)
  | T.null (T.strip l) = EmptyLineItem l term : parseBlock minIndent rest
  | "--" `T.isPrefixOf` (T.stripStart l) = CommentItem l term : parseBlock minIndent rest
  | otherwise = 
      let currentIndent = countIndent l
      in if currentIndent < minIndent 
         then [] -- End of block
         else 
           -- Process the item
           let (item, remaining) = parseItem currentIndent lines'
           in item : parseBlock minIndent remaining

-- | Parse a single logical item (Field, Section, or If)
parseItem :: Int -> [(Text, Text)] -> (CabalItem, [(Text, Text)])
parseItem _ [] = (EmptyLineItem "" "", []) -- Should not be reachable from parseBlock
parseItem currentIndent ((line, term):rest) = 
  let trimmed = T.stripStart line
  in if "if " `T.isPrefixOf` trimmed
     then parseIfBlock currentIndent ((line, term):rest)
     else if isSectionHeader trimmed
          then parseSectionBlock currentIndent ((line, term):rest)
          else parseField currentIndent ((line, term):rest)

-- | Detect section header (library, executable, etc.)
isSectionHeader :: Text -> Bool
isSectionHeader t = 
  let headers = ["library", "executable", "test-suite", "benchmark", "common", "flag", "source-repository", "custom-setup"]
      firstWord = fst $ T.break isSpace t
  in firstWord `elem` headers

parseSectionBlock :: Int -> [(Text, Text)] -> (CabalItem, [(Text, Text)])
parseSectionBlock _ [] = (EmptyLineItem "" "", [])
parseSectionBlock indent ((line, term):rest) = 
  let trimmed = T.stripStart line
      (type', argsRaw) = T.break isSpace trimmed
      (cleanArgs, useBraces) = detectBrace (T.strip argsRaw)
      sLine = SectionLine indent type' cleanArgs term useBraces
      
      (blockLines, remaining) = takeBlock rest indent useBraces
      -- If using braces, ignore indentation for children
      children = if null blockLines 
                 then []
                 else parseBlock (if useBraces then 0 else indent + 1) blockLines
                 
  in (SectionItem sLine children, remaining)

parseIfBlock :: Int -> [(Text, Text)] -> (CabalItem, [(Text, Text)])
parseIfBlock _ [] = (EmptyLineItem "" "", [])
parseIfBlock indent ((line, term):rest) = 
  let trimmed = T.stripStart line
      condRaw = T.strip (T.drop 3 trimmed) -- drop "if "
      (cond, useBraces) = detectBrace condRaw
      ifLine = IfLine indent cond term useBraces
      
      (thenLines, afterThen) = takeBlock rest indent useBraces
      thenItems = parseBlock (if useBraces then 0 else indent + 1) thenLines
      
      -- Check for else
      (elsePart, finalRemaining) = case afterThen of
        ((l, t):rs) | "else" `T.isPrefixOf` (T.stripStart l) ->
             let elseIndentVal = countIndent l
                 elseRaw = T.strip (T.drop 4 (T.stripStart l))
                 (_, elseBraces) = detectBrace elseRaw
                 (elseLines, afterElse) = takeBlock rs indent elseBraces
                 elseItems = parseBlock (if elseBraces then 0 else indent + 1) elseLines
             in (Just (ElseLine elseIndentVal t elseBraces, elseItems), afterElse)
        _ -> (Nothing, afterThen)
        
  in (IfBlock ifLine thenItems elsePart, finalRemaining)

detectBrace :: Text -> (Text, Bool)
detectBrace t =
  let t' = T.stripEnd t
  in if "{" `T.isSuffixOf` t'
     then (T.stripEnd (T.dropEnd 1 t'), True)
     else (t, False)

parseField :: Int -> [(Text, Text)] -> (CabalItem, [(Text, Text)])
parseField _ [] = (EmptyLineItem "" "", [])
parseField indent ((line, term):rest) = 
  let (name, valPart) = T.breakOn ":" (T.stripStart line)
      val = T.drop 1 valPart -- drop ":"
      
      (hangingLines, remaining) = span (isHanging indent) rest
      
      (fullValue, finalTerm) = case hangingLines of
        [] -> (val, term)
        _  -> 
          let hLines = init hangingLines
              (lastL, lastT) = last hangingLines
              v = val <> term <> T.intercalate "" (map (\(l, t) -> l <> t) hLines) <> lastL
          in (v, lastT)
                  
      fLine = FieldLine indent name fullValue finalTerm
  in (FieldItem fLine, remaining)

  where
    isHanging :: Int -> (Text, Text) -> Bool
    isHanging parentIndent (l, _) = 
      let i = countIndent l
      in T.null (T.strip l) || i > parentIndent

takeBlock :: [(Text, Text)] -> Int -> Bool -> ([(Text, Text)], [(Text, Text)])
takeBlock lines' parentIndent False = 
  span (isChild parentIndent) lines'
takeBlock lines' _ True =
  let (block, rest) = break isClosingBrace lines'
      remaining = case rest of
        ((l, term):rs) -> 
           let t = T.stripStart l
           in if t == "}" 
              then rs 
              else (T.stripStart (T.drop 1 t), term) : rs -- Handle '} else'
        [] -> []
  in (block, remaining)

isClosingBrace :: (Text, Text) -> Bool
isClosingBrace (l, _) = 
  let t = T.strip l
  in t == "}" || "}" `T.isPrefixOf` t

isChild :: Int -> (Text, Text) -> Bool
isChild parentIndent (l, _) = 
  let i = countIndent l
  in T.null (T.strip l) || i > parentIndent

countIndent :: Text -> Int
countIndent t = T.length $ T.takeWhile (== ' ') t