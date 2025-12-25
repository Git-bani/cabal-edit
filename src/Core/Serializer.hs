{-# LANGUAGE OverloadedStrings #-}

module Core.Serializer
  ( serializeCabalFile
  , updateDependencies
  , DependencyOperation(..)
  , formatDependency
  , formatDependencyList
  , insertDependencyLine
  , removeDependencyLine
  , updateDependencyLine
  , replaceBuildDependsBlock
  )
where

import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, find)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Distribution.Pretty as CabalPretty

-- Convert CabalFile back to text with modifications
serializeCabalFile :: CabalFile -> Text
serializeCabalFile cf = cfRawContent cf

data DependencyOperation = Add | Remove | Update

-- Update dependencies in the raw content
updateDependencies :: CabalFile -> Bool -> [Dependency] -> DependencyOperation -> CabalFile
updateDependencies cf leadingComma deps op =
  cf { cfRawContent = applyOperation (cfLineEndings cf) leadingComma (cfRawContent cf) deps op }

-- Apply operation to raw content
applyOperation :: Text -> Bool -> Text -> [Dependency] -> DependencyOperation -> Text
applyOperation eol leadingComma content deps Add =
  foldr (insertDependencyLine eol leadingComma) content deps
applyOperation eol leadingComma content deps Remove =
  foldr (removeDependencyLine eol leadingComma) content deps
applyOperation eol leadingComma content deps Update =
  foldr (updateDependencyLine eol leadingComma) content deps

-- Helper to join lines with specific EOL
unlinesWith :: Text -> [Text] -> Text
unlinesWith eol ls = T.concat $ map (<> eol) ls

-- Helper to split lines and remove trailing CR
linesCR :: Text -> [Text]
linesCR = map (T.dropWhileEnd (== '\r')) . T.lines

-- Insert a dependency using Surgical Insertion (preserving comments/structure)
insertDependencyLine :: Text -> Bool -> Dependency -> Text -> Text
insertDependencyLine eol leadingComma newDep content =
  let baseIndent = detectBaseIndent content
      maybeTarget = findMainBuildDepends baseIndent content
  in case maybeTarget of
       Just (indent, before, targetLine, after) ->
         let (depsBlock, afterBlock) = spanIndentedBlock indent after
             blockLines = if T.null depsBlock then [] else linesCR depsBlock
             allLines = targetLine : blockLines
             
             depsWithLoc = parseLinesWithDeps allLines
             
             -- Sort dependencies to find position
             sortedAll = sort (newDep : map fst depsWithLoc)
             
             -- Find neighbors
             maybePrev = findPrev newDep sortedAll
             maybeNext = findNext newDep sortedAll
             
             -- Determine insertion strategy
             insertion = case (maybePrev, maybeNext) of
               (Just prev, _) -> 
                 -- Insert after 'prev'
                 case findLoc prev depsWithLoc of
                   Just prevLineIdx -> InsertAfter prevLineIdx
                   Nothing -> AppendToEnd -- Should not happen
               (Nothing, Just next) ->
                 -- Insert before 'next' (newDep is first)
                 case findLoc next depsWithLoc of
                   Just nextLineIdx -> InsertBefore nextLineIdx
                   Nothing -> AppendToEnd
               (Nothing, Nothing) -> AppendToEnd -- Empty list
               
         in applyInsertion eol leadingComma newDep insertion indent allLines before afterBlock

       Nothing ->
         -- No build-depends found, create new block
         let (before, after) = findInsertionPoint baseIndent content
             indent = baseIndent
             indentStr = T.replicate indent " "
             formatted = formatDependencyList eol leadingComma [newDep] (indent + 2)
             newField = indentStr <> "build-depends:" <> formatted <> eol
         in before <> newField <> after

data InsertionPoint = InsertAfter Int | InsertBefore Int | AppendToEnd
  deriving (Show, Eq)

applyInsertion :: Text -> Bool -> Dependency -> InsertionPoint -> Int -> [Text] -> Text -> Text -> Text
applyInsertion eol leadingComma dep (InsertAfter lineIdx) baseIndent allLines before afterBlock =
  let (pre, post) = splitAt (lineIdx + 1) allLines
      -- Indent should match the line we are inserting after, roughly
      prevIndent = case reverse pre of
        (prevLine:_) -> T.length $ T.takeWhile (== ' ') prevLine
        [] -> baseIndent + 2
      
      -- If prevLine is the "build-depends:" line, we might need more indent
      indentToUse = if lineIdx == 0 then baseIndent + 2 else prevIndent
      indentStr = T.replicate indentToUse " "
      
      newLine = if leadingComma 
                then indentStr <> ", " <> formatDependency dep
                else indentStr <> "  " <> formatDependency dep <> ","
      
      newBlock = unlinesWith eol (pre ++ [newLine] ++ post)
  in before <> newBlock <> afterBlock

applyInsertion eol leadingComma dep (InsertBefore lineIdx) baseIndent allLines before afterBlock =
  let (pre, post) = splitAt lineIdx allLines
      -- We are inserting BEFORE lineIdx.
      
      indentToUse = baseIndent + 2
      indentStr = T.replicate indentToUse " "
      
      newLine = if leadingComma
                then indentStr <> ", " <> formatDependency dep
                else indentStr <> "  " <> formatDependency dep <> ","
      
      -- Handle insertion before line 0 (targetLine)
      newBlock = if lineIdx == 0 
                 then
                   case post of
                     [] -> "" -- Should not be reachable given logic
                     (targetLine:restLines) ->
                       let (key, val) = T.breakOnEnd "build-depends:" targetLine
                           valClean = T.strip val
                           
                           -- New dependency line (no comma, because it's first in the new block)
                           firstLine = indentStr <> "  " <> formatDependency dep
                           
                           -- Old value converted to new line (with comma)
                           movedValLine = if T.null valClean 
                                          then [] 
                                          else if leadingComma 
                                               then [indentStr <> ", " <> valClean]
                                               else [indentStr <> "  " <> valClean <> ","]
                           
                           -- If restLines exists and val was empty, the first line of restLines
                           -- is now second. It should have a comma.
                           finalRest = case restLines of
                             (r:rs) | T.null valClean && not (hasComma r) ->
                               addCommaToFirst leadingComma indentToUse (r:rs)
                             _ -> restLines
                                       
                       in unlinesWith eol ([key, firstLine] ++ movedValLine ++ finalRest)
                 else
                   -- Normal insertion before a line in the block (lineIdx > 0)
                   -- We insert 'newLine' (with comma).
                   -- The line we insert before (head post) MUST keep its comma if it had one.
                   case post of
                     [] -> unlinesWith eol (pre ++ [newLine])
                     (nextLine:_) ->
                       let finalPost = if not (hasComma nextLine)
                                       then addCommaToFirst leadingComma indentToUse post
                                       else post
                       in unlinesWith eol (pre ++ [newLine] ++ finalPost)

  in before <> newBlock <> afterBlock

applyInsertion eol leadingComma dep AppendToEnd baseIndent allLines before afterBlock =
  -- List was empty or we append to end
  -- If empty, lines might just be ["...build-depends:"]
  let lastIndent = case reverse allLines of
        (lastLine:_) -> T.length $ T.takeWhile (== ' ') lastLine
        [] -> baseIndent + 2
      indentToUse = if length allLines == 1 then baseIndent + 2 else lastIndent
      indentStr = T.replicate indentToUse " "
      
      -- Check if we need comma (if list not empty)
      hasExisting = length (parseLinesWithDeps allLines) > 0
      prefix = if hasExisting then (if leadingComma then ", " else "  ") else "  "
      suffix = if hasExisting && not leadingComma then "," else ""
      
      newLine = indentStr <> prefix <> formatDependency dep <> suffix
      newBlock = unlinesWith eol (allLines ++ [newLine])
  in before <> newBlock <> afterBlock

addCommaToFirst :: Bool -> Int -> [Text] -> [Text]
addCommaToFirst leadingComma indent (l:ls) =
  let trimmed = T.strip l
  in if "build-depends:" `T.isInfixOf` l
     then 
       l : ls
     else 
       if leadingComma 
       then (T.replicate indent " " <> ", " <> trimmed) : ls
       else (T.replicate indent " " <> "  " <> trimmed <> ",") : ls
addCommaToFirst _ _ [] = []

hasComma :: Text -> Bool
hasComma t = "," `T.isPrefixOf` (T.strip t)

-- Parser helpers

parseLinesWithDeps :: [Text] -> [(Dependency, Int)]
parseLinesWithDeps ls =
  concatMap parseLine (zip [0..] ls)
  where
    parseLine (idx, line) =
      let clean = if idx == 0 then T.replace "build-depends:" "" line else line
          parts = T.splitOn "," clean
          deps = mapMaybe parseSingleDep parts
      in map (\d -> (d, idx)) deps

findLoc :: Dependency -> [(Dependency, Int)] -> Maybe Int
findLoc d list = fmap snd $ find (\(d', _) -> depName d' == depName d) list

findPrev :: Ord a => a -> [a] -> Maybe a
findPrev _ [] = Nothing
findPrev target (x:xs)
  | x == target = Nothing
  | otherwise = case xs of
      [] -> Just x
      (y:_) | y == target -> Just x
            | otherwise -> findPrev target xs

findNext :: Ord a => a -> [a] -> Maybe a
findNext _ [] = Nothing
findNext target list =
  case dropWhile (/= target) list of
    (_:y:_)
      | y == target -> Just y
    (_:y:_) -> Just y
    _ -> Nothing


-- ... Keep existing helpers ...

detectBaseIndent :: Text -> Int
detectBaseIndent content =
  let ls = filter (not . T.null . T.strip) $ linesCR content
      validLines = filter isFieldLine ls
  in case validLines of
       (l:_) -> T.length $ T.takeWhile (== ' ') l
       [] -> 2 

isFieldLine :: Text -> Bool
isFieldLine t =
  let trimmed = T.strip t
  in not ("--" `T.isPrefixOf` trimmed) && 
     not ("if " `T.isPrefixOf` trimmed) && 
     not ("else" `T.isPrefixOf` trimmed) &&
     not ("import:" `T.isPrefixOf` trimmed) &&
     not ("common " `T.isPrefixOf` trimmed) &&
     T.isInfixOf ":" trimmed

findMainBuildDepends :: Int -> Text -> Maybe (Int, Text, Text, Text)
findMainBuildDepends baseIndent content =
  let ls = linesCR content
      matchIndex = findIndexMatches 0 ls
  in case matchIndex of
       Nothing -> Nothing
       Just idx -> 
         let (beforeLines, matchAndRest) = splitAt idx ls
         in case matchAndRest of 
              (targetLine:afterLines) -> 
                let indent = T.length $ T.takeWhile (== ' ') targetLine
                in Just (indent, unlinesWith "\n" beforeLines, targetLine, unlinesWith "\n" afterLines)
              [] -> Nothing
  where
    findIndexMatches :: Int -> [Text] -> Maybe Int
    findIndexMatches _ [] = Nothing
    findIndexMatches idx (l:ls') =
      let trimmed = T.strip l
          currentIndent = T.length $ T.takeWhile (== ' ') l
      in if "--" `T.isPrefixOf` trimmed
         then findIndexMatches (idx + 1) ls' 
         else if "build-depends:" `T.isInfixOf` l && currentIndent == baseIndent
              then Just idx
              else findIndexMatches (idx + 1) ls'

findInsertionPoint :: Int -> Text -> (Text, Text)
findInsertionPoint baseIndent content =
  let ls = linesCR content
      zipped = zip [0..] ls
      (header, rest) = span (\(_, l) -> T.length (T.takeWhile (== ' ') l) < baseIndent && not (T.null (T.strip l))) zipped
      endIndex = findIndexEnd baseIndent rest
      splitIdx = length header + endIndex
      (beforeLines, afterLines) = splitAt splitIdx ls
  in (unlinesWith "\n" beforeLines, unlinesWith "\n" afterLines)

findIndexEnd :: Int -> [(Int, Text)] -> Int
findIndexEnd _ [] = 0
findIndexEnd baseIndent ((_, line):rest) =
  let trimmed = T.strip line
      indent = T.length $ T.takeWhile (== ' ') line
  in if T.null trimmed
     then 1 + findIndexEnd baseIndent rest
     else if indent < baseIndent 
     then 0 
     else 
       if any (`T.isPrefixOf` trimmed) ["if ", "common ", "else", "import:"] && indent == baseIndent
       then 0 
       else 1 + findIndexEnd baseIndent rest

replaceBuildDependsBlock :: Text -> Bool -> [Dependency] -> Text -> Text
replaceBuildDependsBlock eol leadingComma newDeps content =
  let baseIndent = detectBaseIndent content
      maybeTarget = findMainBuildDepends baseIndent content
  in case maybeTarget of
       Just (indent, before, _, after) -> 
         let (depsBlock, afterBlock) = spanIndentedBlock indent after
             formatted = formatDependencyList eol leadingComma (sort newDeps) (if T.null (T.strip depsBlock) then indent + 2 else detectIndentation depsBlock)
             indentStr = T.replicate indent " "
         in before <> indentStr <> "build-depends:" <> formatted <> eol <> afterBlock
       Nothing -> content 

spanIndentedBlock :: Int -> Text -> (Text, Text)
spanIndentedBlock keyIndent text =
  let ls = linesCR text
      (block, rest) = extractBlock ls
  in (unlinesWith "\n" block, unlinesWith "\n" rest)
  where
    extractBlock :: [Text] -> ([Text], [Text])
    extractBlock [] = ([], [])
    extractBlock (l:ls) =
      let trimmed = T.strip l
          indent = T.length (T.takeWhile (== ' ') l)
          isNewField = indent <= keyIndent + 1 && ":" `T.isInfixOf` l
          isIndented = indent > keyIndent
      in if T.null trimmed
         then case extractBlock ls of 
                ([], rest) -> ([], l:rest) 
                (b, r)     -> (l:b, r)     
         else if isIndented && not isNewField
              then let (b, r) = extractBlock ls in (l:b, r)
              else ([], l:ls)

removeDependencyLine :: Text -> Bool -> Dependency -> Text -> Text
removeDependencyLine eol leadingComma dep content =
  let baseIndent = detectBaseIndent content
      maybeTarget = findMainBuildDepends baseIndent content
  in case maybeTarget of
       Just (indent, before, targetLine, after) -> 
         let (depsBlock, afterBlock) = spanIndentedBlock indent after
             blockLines = if T.null depsBlock then [] else linesCR depsBlock
             allLines = targetLine : blockLines
             
             depsWithLoc = parseLinesWithDeps allLines
             maybeLoc = findLoc dep depsWithLoc
             
         in case maybeLoc of 
              Just idx -> 
                 -- Remove line at idx
                 if idx == 0 
                 then
                   -- Removing from targetLine (inline)
                   let line0Deps = filter (\(_, i) -> i == 0) depsWithLoc
                       otherDepsOnLine0 = filter (\(d, _) -> depName d /= depName dep) line0Deps
                   in if null otherDepsOnLine0
                      then
                        if null blockLines
                        then before <> indentStr <> "build-depends:" <> eol <> afterBlock 
                        else
                          before <> indentStr <> "build-depends:" <> eol <> depsBlock <> afterBlock
                      else
                         -- Other deps on line 0. Reconstruct line 0.
                         let newLine0Deps = map fst otherDepsOnLine0
                             formatted0 = T.intercalate ", " (map formatDependency newLine0Deps)
                             newLine0 = indentStr <> "build-depends: " <> formatted0
                         in before <> newLine0 <> eol <> depsBlock <> afterBlock
                 else
                   -- Removing a line from the block (idx > 0)
                   let (pre, post) = splitAt (idx - 1) blockLines
                       finalBlockLines = pre ++ drop 1 post
                       
                       -- Adjust comma for the NEW first line of the block if necessary
                       -- If we removed the first line of the block, the new first line might need comma removal (if trailing)
                       -- or comma addition?
                       -- Actually, the current removal logic is simple.
                       -- Let's just fix the unlines logic.
                       finalBlock = unlinesWith eol finalBlockLines
                   in before <> targetLine <> eol <> finalBlock <> afterBlock
              
              Nothing -> content -- Dep not found
         where
           indentStr = T.replicate indent " "
              
       Nothing -> content

updateDependencyLine :: Text -> Bool -> Dependency -> Text -> Text
updateDependencyLine eol leadingComma dep content =
  insertDependencyLine eol leadingComma dep (removeDependencyLine eol leadingComma dep content)

parseSingleDep :: Text -> Maybe Dependency
parseSingleDep depStr =
  let trimmed = T.strip depStr
      (name, constraint) = T.breakOn " " trimmed
      nameClean = T.strip name
   in if T.null nameClean || "--" `T.isPrefixOf` nameClean
        then Nothing
        else Just $ Dependency
          {
            depName = unsafeMkPackageName nameClean
          , depVersionConstraint = parseVersionConstraint (T.strip constraint)
          , depType = BuildDepends
          }

formatDependency :: Dependency -> Text
formatDependency dep =
  unPackageName (depName dep) <> formatVersionConstraint (depVersionConstraint dep)

formatVersionConstraint :: Maybe VersionConstraint -> Text
formatVersionConstraint Nothing = ""
formatVersionConstraint (Just AnyVersion) = ""
formatVersionConstraint (Just (ExactVersion ver)) =
  " ==" <> formatVersion ver
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

formatDependencyList :: Text -> Bool -> [Dependency] -> Int -> Text
formatDependencyList eol leadingComma deps indent =
  let sortedDeps = sort deps 
      formattedNames = map formatDependency sortedDeps
      indentStr = T.replicate indent " "
      
      formatEntry :: (Int, Text) -> Text
      formatEntry (0, d) = 
        let suffix = if not leadingComma && length formattedNames > 1 then "," else ""
        in eol <> indentStr <> "  " <> d <> suffix
      formatEntry (idx, d) = 
        let isLast = idx == length formattedNames - 1
            prefix = if leadingComma then ", " else "  "
            suffix = if not leadingComma && not isLast then "," else ""
        in eol <> indentStr <> prefix <> d <> suffix
      
   in T.concat $ map formatEntry (zip [0..] formattedNames)

detectIndentation :: Text -> Int
detectIndentation content =
  let ls = take 5 $ filter (not . T.null) $ T.lines content
      leadingSpaces = map (T.length . T.takeWhile (== ' ')) ls
      nonZeroSpaces = filter (> 0) leadingSpaces
   in case nonZeroSpaces of
        [] -> 4 
        spaces -> minimum spaces

parseVersionConstraint :: Text -> Maybe VersionConstraint
parseVersionConstraint "" = Nothing
parseVersionConstraint constraint =
  if T.isPrefixOf "==" constraint
  then Just $ ExactVersion $ parseVersionText $ T.drop 2 constraint
  else Just $ UnparsedVersion constraint 

parseVersionText :: Text -> Version
parseVersionText versionText =
  let parts = T.splitOn "." (T.strip versionText)
      nums = mapMaybe (readMaybe . T.unpack) parts
   in Version nums