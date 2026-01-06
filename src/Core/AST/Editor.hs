{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Core.AST.Editor
  ( addDependencyToAST
  , removeDependencyFromAST
  , updateDependencyInAST
  , updateFieldInAST
  , findDependencyInAST
  , findDependenciesInAST
  , findFlagStanzasInAST
  , addFlagToAST
  , updateFlagDefaultInAST
  , removeSectionFromAST
  )
where

import Core.AST.Types
import Core.Types (Dependency(..), mkPackageName, unPackageName, Result(..), Error(..), ErrorCode(..), DependencyType(..), VersionConstraint(..), Version(..))
import Core.AST.Serializer (formatDependency)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find, findIndex)
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (mapMaybe, isNothing)
import Text.Read (readMaybe)

isVersionOpStart :: Char -> Bool
isVersionOpStart c = c `elem` ("> <=^" :: String)

-- | Add a dependency to a specific target in the AST, optionally inside an 'if' block
addDependencyToAST :: Text -> Maybe Text -> Dependency -> CabalAST -> Result CabalAST
addDependencyToAST targetSection mCondition dep (CabalAST items) = 
  let (updatedItems, found) = runUpdateFilter (updateSection targetSection mCondition (addDepToItems dep)) items
  in if found 
     then Success $ CabalAST updatedItems
     else Failure $ Error ("Section not found: " <> targetSection <> maybe "" (" with condition " <>) mCondition) InvalidDependency

-- | Update a dependency version in a specific target
updateDependencyInAST :: Text -> Maybe Text -> Dependency -> CabalAST -> Result CabalAST
updateDependencyInAST targetSection mCondition dep (CabalAST items) = 
  let (updatedItems, found) = runUpdateFilter (updateSection targetSection mCondition (addDepToItems dep)) items
  in if found 
     then Success $ CabalAST updatedItems
     else Failure $ Error ("Section not found: " <> targetSection <> maybe "" (" with condition " <>) mCondition) InvalidDependency

-- | Update a specific field value globally (useful for top-level fields like version)
updateFieldInAST :: Text -> Text -> CabalAST -> Result CabalAST
updateFieldInAST name newVal (CabalAST items) = 
  let (updatedItems, found) = runUpdate (updateField name newVal) items
  in if found 
     then Success $ CabalAST updatedItems
     else Failure $ Error ("Field not found: " <> name) ParseError

updateField :: Text -> Text -> CabalItem -> (CabalItem, Bool)
updateField targetName newVal (FieldItem fl)
  | T.toLower (fieldName fl) == T.toLower targetName = 
      (FieldItem (fl { fieldValue = " " <> newVal }), True)
updateField targetName newVal (SectionItem sl children) =
  let (updatedChildren, found) = runUpdate (updateField targetName newVal) children
  in (SectionItem sl updatedChildren, found)
updateField targetName newVal (IfBlock il thenItems elsePart) =
  let (updatedThen, foundThen) = runUpdate (updateField targetName newVal) thenItems
      (updatedElse, foundElse) = case elsePart of
        Just (el, items) -> 
          let (ui, fnd) = runUpdate (updateField targetName newVal) items
          in (Just (el, ui), fnd)
        Nothing -> (Nothing, False)
  in (IfBlock il updatedThen updatedElse, foundThen || foundElse)
updateField _ _ item = (item, False)

isFieldNamed :: Text -> CabalItem -> Bool
isFieldNamed name (FieldItem fl) = T.toLower (fieldName fl) == T.toLower name
isFieldNamed _ _ = False

-- | Find all flags in the AST
findFlagStanzasInAST :: CabalAST -> [(Text, Bool)]
findFlagStanzasInAST (CabalAST items) = 
  mapMaybe getFlag items
  where
    getFlag (SectionItem sl children) 
      | T.toLower (sectionType sl) == "flag" = 
          let def = findField "default" children
              defVal = case def of
                         Just v -> T.toLower (T.strip v) `elem` ["true", "yes"]
                         Nothing -> False
          in Just (sectionArgs sl, defVal)
    getFlag _ = Nothing
    
    findField name children = 
      case find (isFieldNamed name) children of
        Just (FieldItem fl) -> Just (fieldValue fl)
        _ -> Nothing

-- | Add a new flag to the AST
addFlagToAST :: Text -> CabalAST -> Result CabalAST
addFlagToAST name (CabalAST items) =
  let newStanza = SectionItem (SectionLine 0 "flag" name)
        [ FieldItem (FieldLine 4 "description" (" " <> name))
        , FieldItem (FieldLine 4 "manual" " True")
        , FieldItem (FieldLine 4 "default" " False")
        , EmptyLineItem ""
        ]
      -- Find a good place to insert (before library or first section)
      idx = case findIndex isSection items of
              Just i -> i
              Nothing -> length items
      updatedItems = take idx items ++ [newStanza] ++ drop idx items
  in Success $ CabalAST updatedItems

isSection :: CabalItem -> Bool
isSection (SectionItem _ _) = True
isSection _ = False

-- | Update a flag's default value
updateFlagDefaultInAST :: Text -> Bool -> CabalAST -> Result CabalAST
updateFlagDefaultInAST name val (CabalAST items) =
  let (updatedItems, found) = runUpdate (updateSpecificFlag name val) items
  in if found 
     then Success $ CabalAST updatedItems
     else Failure $ Error ("Flag not found: " <> name) FileNotFound

updateSpecificFlag :: Text -> Bool -> CabalItem -> (CabalItem, Bool)
updateSpecificFlag targetName val (SectionItem sl children)
  | T.toLower (sectionType sl) == "flag" && T.toLower (sectionArgs sl) == T.toLower targetName =
      let valStr = if val then "True" else "False"
          (updatedChildren, found) = runUpdate (updateField "default" valStr) children
          -- If 'default' field not found, add it
          finalChildren = if found then updatedChildren else updatedChildren ++ [FieldItem (FieldLine 4 "default" (" " <> valStr))]
      in (SectionItem sl finalChildren, True)
updateSpecificFlag _ _ item = (item, False)

-- | Remove a section (e.g. flag) from the AST
removeSectionFromAST :: Text -> Text -> CabalAST -> Result CabalAST
removeSectionFromAST sType sArgs (CabalAST items) =
  let targetHeader = sType <> (if T.null sArgs then "" else " " <> sArgs)
      (updatedItems, found) = runUpdateFilter (removeSpecificSection sType sArgs) items
  in if found
     then Success $ CabalAST updatedItems
     else Failure $ Error ("Section not found: " <> targetHeader) FileNotFound

removeSpecificSection :: Text -> Text -> CabalItem -> (Maybe CabalItem, Bool)
removeSpecificSection tType tArgs (SectionItem sl _)
  | T.toLower (sectionType sl) == T.toLower tType && T.toLower (sectionArgs sl) == T.toLower tArgs =
      (Nothing, True) 
removeSpecificSection _ _ item = (Just item, False)

-- | Remove a dependency from a specific target in the AST
removeDependencyFromAST :: Text -> Maybe Text -> Text -> CabalAST -> Result CabalAST
removeDependencyFromAST targetSection mCondition pkgName (CabalAST items) = 
  let (updatedItems, found) = runUpdateFilter (updateSection targetSection mCondition (removeDepFromItems pkgName)) items
  in if found 
     then Success $ CabalAST updatedItems
     else Failure $ Error ("Section not found: " <> targetSection <> maybe "" (" with condition " <>) mCondition) InvalidDependency

-- | Find all (SectionName, Maybe Condition) pairs where a package is used
findDependencyInAST :: Text -> CabalAST -> [(Text, Maybe Text)]
findDependencyInAST pkgName ast = 
  map (\(s, c, _) -> (s, c)) $ filter (\(_, _, d) -> unPackageName (depName d) == pkgName) (findDependenciesInAST ast)

-- | Find all dependencies in the AST with their locations
findDependenciesInAST :: CabalAST -> [(Text, Maybe Text, Dependency)]
findDependenciesInAST (CabalAST items) = 
  concatMap (findInItem Nothing) items
  where
    findInItem _ (SectionItem sl children) =
      let secName = describeSectionLine sl
      in concatMap (findInItemUnder secName Nothing) children
    findInItem _ _ = []

    findInItemUnder secName mCond (FieldItem fl)
      | isBuildDepends (FieldItem fl) = 
          let deps = parseFieldDependencies (fieldValue fl)
          in map (secName, mCond, ) deps
      | otherwise = []
    findInItemUnder secName mCond (IfBlock il thenItems mElse) =
      let cond = ifCondition il
          combinedCond = case mCond of
                           Nothing -> Just cond
                           Just c -> Just (c <> " && " <> cond) 
          thenMatches = concatMap (findInItemUnder secName combinedCond) thenItems
          elseMatches = case mElse of
                          Just (_, eItems) -> concatMap (findInItemUnder secName mCond) eItems 
                          Nothing -> []
      in thenMatches ++ elseMatches
    findInItemUnder _ _ _ = []

parseFieldDependencies :: Text -> [Dependency]
parseFieldDependencies val =
  let ls = T.splitOn "\n" val
      parts = concatMap (T.splitOn ",") ls
  in mapMaybe parseDependencyText parts

parseDependencyText :: Text -> Maybe Dependency
parseDependencyText depStr =
  let trimmed = T.strip depStr
      -- Remove leading comma if present (from leading comma style)
      cleanDep = if "," `T.isPrefixOf` trimmed then T.strip (T.drop 1 trimmed) else trimmed
      (name, constraint) = T.break (\c -> isSpace c || isVersionOpStart c) cleanDep
      nameClean = T.strip name
   in if T.null nameClean || "--" `T.isPrefixOf` nameClean
        then Nothing
        else case mkPackageName nameClean of
               Left _ -> Nothing
               Right pkgName ->
                 Just $ Dependency
                   {
                     depName = pkgName
                   , depVersionConstraint = parseVersionConstraint' (T.strip constraint)
                   , depType = BuildDepends
                   }

parseVersionConstraint' :: Text -> Maybe VersionConstraint
parseVersionConstraint' "" = Nothing
parseVersionConstraint' constraint =
  if T.isPrefixOf "==" constraint
  then Just $ ExactVersion $ parseVersionText' $ T.drop 2 constraint
  else Just $ UnparsedVersion constraint 

parseVersionText' :: Text -> Version
parseVersionText' versionText =
  let parts = T.splitOn "." (T.strip versionText)
      nums = mapMaybe (readMaybe' . T.unpack) parts
   in Version nums

readMaybe' :: String -> Maybe Int
readMaybe' = readMaybe

describeSectionLine :: SectionLine -> Text
describeSectionLine sl = 
  sectionType sl <> (if T.null (sectionArgs sl) then "" else " " <> sectionArgs sl)

runUpdate :: (a -> (a, Bool)) -> [a] -> ([a], Bool)
runUpdate f xs = 
  let results = map f xs
  in (map fst results, any snd results)

runUpdateFilter :: (a -> (Maybe a, Bool)) -> [a] -> ([a], Bool)
runUpdateFilter f xs = 
  let results = map f xs
      items = mapMaybe fst results
      found = any snd results
  in (items, found)

--------------------------------------------------------------------------------
-- Traversal Helpers
--------------------------------------------------------------------------------

-- | Recursively find and update a section
updateSection :: Text -> Maybe Text -> ([CabalItem] -> [CabalItem]) -> CabalItem -> (Maybe CabalItem, Bool)
updateSection target mCond f (SectionItem sl children)
  | matchesTarget target sl = 
      case mCond of
        Nothing -> (Just (SectionItem sl (f children)), True)
        Just cond -> 
          let (updatedChildren, found) = runUpdateFilter (updateIfBlock cond f) children
          in if found 
             then (Just (SectionItem sl updatedChildren), True)
             else -- Create new if block if not found
               let newIf = IfBlock (IfLine (sectionIndent sl + 4) cond) (f []) Nothing
               in (Just (SectionItem sl (children ++ [newIf])), True)
  | otherwise = 
      let (updatedChildren, found) = runUpdateFilter (updateSection target mCond f) children
      in (Just (SectionItem sl updatedChildren), found)
updateSection _ _ _ item = (Just item, False)

updateIfBlock :: Text -> ([CabalItem] -> [CabalItem]) -> CabalItem -> (Maybe CabalItem, Bool)
updateIfBlock targetCond f (IfBlock il thenItems elsePart)
  | matchesCondition targetCond il = 
      let newThen = f thenItems
          -- Check if block should be removed
          shouldRemove = isEmptyBlock newThen && isNothing elsePart
      in if shouldRemove 
         then (Nothing, True)
         else (Just (IfBlock il newThen elsePart), True)
  | otherwise = 
      let (updatedThen, foundThen) = runUpdateFilter (updateIfBlock targetCond f) thenItems
          (updatedElse, foundElse) = case elsePart of
            Just (el, items) -> 
              let (ui, fnd) = runUpdateFilter (updateIfBlock targetCond f) items
              in (Just (el, ui), fnd)
            Nothing -> (Nothing, False)
      in (Just (IfBlock il updatedThen updatedElse), foundThen || foundElse)
updateIfBlock _ _ item = (Just item, False)

isEmptyBlock :: [CabalItem] -> Bool
isEmptyBlock = all isEmptyItem

isEmptyItem :: CabalItem -> Bool
isEmptyItem (EmptyLineItem _) = True
isEmptyItem (FieldItem fl) = T.null (T.strip (fieldValue fl)) && T.isPrefixOf "--" (T.strip (fieldName fl)) -- Consider comment-only fields as empty? No, fieldName shouldn't be comment.
isEmptyItem _ = False

matchesCondition :: Text -> IfLine -> Bool
matchesCondition target il = 
  T.toLower (T.strip target) == T.toLower (T.strip (ifCondition il))

matchesTarget :: Text -> SectionLine -> Bool
matchesTarget target sl = 
  let full = sectionType sl <> (if T.null (sectionArgs sl) then "" else " " <> sectionArgs sl)
      t = T.toLower (T.strip target)
      f = T.toLower (T.strip full)
      st = T.toLower (T.strip (sectionType sl))
      sa = T.toLower (T.strip (sectionArgs sl))
  in t == f || t == sa || (t == st && T.null sa)

--------------------------------------------------------------------------------
-- Field Manipulation
--------------------------------------------------------------------------------

addDepToItems :: Dependency -> [CabalItem] -> [CabalItem]
addDepToItems dep items = 
  let items' = ensureBuildDepends items
  in case findIndex isBuildDepends items' of
    Just idx -> 
      case items' !! idx of
        FieldItem fl -> 
          let updatedField = addOrUpdateDepInField dep fl
          in take idx items' ++ [FieldItem updatedField] ++ drop (idx + 1) items'
        _ -> items'
    Nothing -> items'

ensureBuildDepends :: [CabalItem] -> [CabalItem]
ensureBuildDepends items =
  if any isBuildDepends items
  then items
  else 
    -- Find a good place to insert build-depends (after last field or at start)
    -- For now, just append it
    let indent = detectBaseIndentAST items
        newField = FieldItem (FieldLine indent "build-depends" "")
    in items ++ [newField]

detectBaseIndentAST :: [CabalItem] -> Int
detectBaseIndentAST items =
  case find isField items of
    Just (FieldItem fl) -> fieldIndent fl
    _ -> 4

isField :: CabalItem -> Bool
isField (FieldItem _) = True
isField _ = False

removeDepFromItems :: Text -> [CabalItem] -> [CabalItem]
removeDepFromItems pkgName items = 
  case findIndex isBuildDepends items of
    Just idx -> 
      case items !! idx of
        FieldItem fl -> 
          let updatedField = removeDepFromField pkgName fl
          in if T.null (T.strip (fieldValue updatedField))
             then take idx items ++ drop (idx + 1) items 
             else take idx items ++ [FieldItem updatedField] ++ drop (idx + 1) items
        _ -> items
    Nothing -> items

isBuildDepends :: CabalItem -> Bool
isBuildDepends (FieldItem fl) = T.toLower (fieldName fl) == "build-depends"
isBuildDepends _ = False

--------------------------------------------------------------------------------
-- Smart Formatting Logic
--------------------------------------------------------------------------------

addOrUpdateDepInField :: Dependency -> FieldLine -> FieldLine
addOrUpdateDepInField dep fl = 
  let val = fieldValue fl
      pkgName = unPackageName (depName dep)
  in if isPkgInText pkgName val
     then updateExistingDep dep fl
     else addNewDep dep fl

isPkgInText :: Text -> Text -> Bool
isPkgInText pkg txt = 
  let parts = concatMap (T.splitOn ",") (T.splitOn "\n" txt)
  in any (isPkgInPart pkg) parts

isPkgInPart :: Text -> Text -> Bool
isPkgInPart pkg part =
  let trimmed = T.strip part
      word = T.takeWhile isPkgChar trimmed
      rest = T.stripStart $ T.drop (T.length word) trimmed
      isValidNext = T.null rest || (case T.uncons rest of
                                     Just (c, _) -> isVersionOpStart c || isSpace c
                                     Nothing -> True)
  in not (T.null word) && T.toLower pkg == T.toLower word && isValidNext

isPkgInLine :: Text -> Text -> Bool
isPkgInLine = isPkgInPart 

isPkgChar :: Char -> Bool
isPkgChar c = isAlphaNum c || c == '-' || c == ':'

addNewDep :: Dependency -> FieldLine -> FieldLine
addNewDep dep fl = 
  let val = fieldValue fl
      lines' = T.splitOn "\n" val
      baseIndent = fieldIndent fl
  in case lines' of
    [s] -> 
      let depStr = formatDependency dep
          newVal = if T.null (T.strip s) 
                   then " " <> depStr 
                   else s <> ", " <> depStr
      in fl { fieldValue = newVal }
    (firstLine:restLines) ->
      let nonCommentRest = filter (not . T.isPrefixOf "--" . T.stripStart) restLines
          style = if any (T.isPrefixOf "," . T.stripStart) nonCommentRest then Leading else Trailing
          indent = detectCommonIndent baseIndent firstLine restLines
          isLeading = case style of { Leading -> True; Trailing -> False }
          -- actualIndent is the column where the package name OR leading comma starts.
          -- If isLeading, we want comma at (indent - 2) if it was aligned to package previously, 
          -- or at indent if it was already aligned to comma.
          actualIndent = if indent <= baseIndent 
                         then baseIndent + 4 
                         else indent
          
          val' = if not isLeading 
                 then ensureTrailingComma val 
                 else val
          
          -- Manually format the new dependency line
          depStr = if isLeading 
                   then T.replicate (actualIndent - 2) " " <> ", " <> formatDependency dep
                   else T.replicate actualIndent " " <> formatDependency dep

          newVal = if T.isSuffixOf "\n" val'
                   then val' <> depStr <> "\n"
                   else val' <> "\n" <> depStr
      in fl { fieldValue = newVal }

ensureTrailingComma :: Text -> Text
ensureTrailingComma t = 
  let ls = T.splitOn "\n" t
      -- Find last non-empty, non-comment line
      isRelevant l = not (T.null (T.strip l)) && not ("--" `T.isPrefixOf` T.stripStart l)
      idx = case findIndex isRelevant (reverse ls) of
              Just i -> length ls - 1 - i
              Nothing -> -1
  in if idx == -1 
     then t
     else 
       let target = ls !! idx
       in if "," `T.isSuffixOf` T.stripEnd target
          then t
          else 
            let (pre, post) = splitAt idx ls
                newTarget = T.dropWhileEnd isSpace target <> ","
            in T.intercalate "\n" (pre ++ [newTarget] ++ drop 1 post)

updateExistingDep :: Dependency -> FieldLine -> FieldLine
updateExistingDep dep fl = 
  let val = fieldValue fl
      pkgName = unPackageName (depName dep)
      ls = T.splitOn "\n" val
      newLs = map (updateSpecificLine pkgName dep) ls
  in fl { fieldValue = T.intercalate "\n" newLs }

updateSpecificLine :: Text -> Dependency -> Text -> Text
updateSpecificLine pkgName dep line = 
  let (indent, rest) = T.span (== ' ') line
      parts = T.splitOn "," rest
      updatePart p = if isPkgInLine pkgName p
                     then let (prefix, _) = T.span (== ' ') p
                          in prefix <> formatDependency dep
                     else p
      newParts = map updatePart parts
  in indent <> T.intercalate "," newParts

removeDepFromField :: Text -> FieldLine -> FieldLine
removeDepFromField pkgName fl = 
  let val = fieldValue fl
      hasTrailingNewline = T.isSuffixOf "\n" val
      ls = T.splitOn "\n" val
      -- Process each line to remove the package if it exists
      processedLines = map (removePkgFromLine pkgName) ls
      -- Filter out Nothing (lines that became empty due to removal)
      -- Keep Just "" (lines that were originally empty or just comments/separators we want to keep? No, removePkgFromLine handles that)
      filteredLines = mapMaybe id processedLines
      
      -- Fix commas
      finalLs = fixCommas filteredLines
      result = T.intercalate "\n" finalLs
      
      resultWithNewline = if hasTrailingNewline && not (T.null result) then result <> "\n" else result

      -- Preserve leading space if it is a single line (or became one) and needs separation
      -- If result has no newlines, it is a single line.
      isSingleLine = not (T.isInfixOf "\n" (T.strip resultWithNewline))
      
      finalResult = if isSingleLine && not (T.null (T.strip resultWithNewline)) && not (" " `T.isPrefixOf` resultWithNewline) && not ("\n" `T.isPrefixOf` resultWithNewline)
                    then " " <> resultWithNewline
                    else if T.null (T.strip resultWithNewline) then "" else resultWithNewline
  in fl { fieldValue = finalResult }

removePkgFromLine :: Text -> Text -> Maybe Text
removePkgFromLine pkg line =
  let (indent, rest) = T.span (== ' ') line
      parts = T.splitOn "," rest
      -- Check if pkg is in this line
      containsPkg = any (isPkgInLine pkg) parts
  in if not containsPkg
     then Just line -- Keep line as is (including empty lines)
     else 
       let remainingParts = filter (not . isPkgInLine pkg) parts
           isLeading = "," `T.isPrefixOf` T.stripStart rest
           newContent = T.intercalate ", " (map T.strip remainingParts)
       in if T.null (T.strip newContent)
          then Nothing -- Remove line entirely
          else 
            if isLeading && not ("," `T.isPrefixOf` T.stripStart newContent)
            then Just $ indent <> ", " <> T.stripStart newContent
            else Just $ indent <> newContent

fixCommas :: [Text] -> [Text]
fixCommas [] = []
fixCommas ls = 
  let isLeading = case detectStyle ls of { Leading -> True; _ -> False }
  in if isLeading 
     then fixLeadingCommas ls 
     else fixTrailingCommas ls

fixTrailingCommas :: [Text] -> [Text]
fixTrailingCommas [] = []
fixTrailingCommas [l] = [T.dropWhileEnd (== ',') l]
fixTrailingCommas (l:ls) = 
  let rest = fixTrailingCommas ls
  in if not (T.null (T.strip l)) && all (T.null . T.strip) rest
     then T.dropWhileEnd (== ',') l : rest
     else l : rest

fixLeadingCommas :: [Text] -> [Text]
fixLeadingCommas [] = []
fixLeadingCommas (l:ls) = 
  let (indent, rest) = T.span (== ' ') l
      trimmed = T.stripStart rest
  in if "," `T.isPrefixOf` trimmed
     then (indent <> T.stripStart (T.drop 1 trimmed)) : ls 
     else l : ls

data Style = Leading | Trailing

detectStyle :: [Text] -> Style
detectStyle ls = 
  if any (T.isPrefixOf "," . T.stripStart) ls
  then Leading
  else Trailing

detectCommonIndent :: Int -> Text -> [Text] -> Int
detectCommonIndent baseIndent firstLine restLines = 
  case find (not . T.null . T.strip) restLines of
    Just l -> 
      let (indentPart, content) = T.span (== ' ') l
          trimmed = T.stripStart content
      in if "," `T.isPrefixOf` trimmed
         then T.length indentPart
         else T.length indentPart
    Nothing -> 
      -- If only one line exists, indent relative to 'build-depends:'
      -- firstLine is just the VALUE of the field, e.g. " base"
      let spacesBeforeValue = T.length $ T.takeWhile (== ' ') firstLine
          -- build-depends is 13 chars, plus 1 for colon
          valIndent = baseIndent + 13 + 1 + spacesBeforeValue
      in valIndent
