{-# LANGUAGE OverloadedStrings #-}

module Core.SerializerSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Core.Serializer
import Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "Serializer.formatDependencyList" $ do
    it "formats with leading commas" $ do
      let deps = [ Dependency (trustedMkPackageName "package-a") Nothing BuildDepends
                 , Dependency (trustedMkPackageName "package-b") Nothing BuildDepends
                 ]
      let result = formatDependencyList "\n" True deps 2
      T.unpack result `shouldContain` ", package-b"
      T.unpack result `shouldContain` "  package-a"
      
    it "ignores commented-out build-depends" $ do
      let content = T.unlines
            [ "library"
            , "  -- build-depends: old-base"
            , "  build-depends: base"
            ]
      let dep = Dependency (trustedMkPackageName "text") Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      
      -- Should insert into the real block
      -- Note: Surgical insertion preserves inline base if inserting after
      result `shouldSatisfy` ("build-depends: base" `T.isInfixOf`)
      result `shouldSatisfy` ("    , text" `T.isInfixOf`)
      
      -- Should NOT modify the comment
      result `shouldSatisfy` ("-- build-depends: old-base" `T.isInfixOf`)

    it "creates new block if only commented one exists" $ do
      let content = T.unlines
            [ "library"
            , "  -- build-depends: old-base"
            ]
      let dep = Dependency (trustedMkPackageName "text") Nothing BuildDepends
      let result = insertDependencyLine "\n" True dep content
      
      -- Should create a new block
      -- Expect 6 spaces: 2 (base) + 2 (nested) + 2 (alignment)
      result `shouldSatisfy` ("\n  build-depends:\n      text" `T.isInfixOf`)

  describe "Serializer Properties (Fuzzing)" $ do
    
    it "Property: formatDependencyList always adheres to leading comma style" $ hedgehog $ do
      deps <- forAll genDependencies
      indent <- forAll $ Gen.int (Range.linear 2 8)
      
      let result = formatDependencyList "\n" True deps indent
      let ls = filter (not . T.null . T.strip) $ T.lines result
      
      annotateShow result
      
      if length deps > 1
        then do
          -- Skip the first line (first dep)
          let subsequent = drop 1 ls
          mapM_ (\l -> do
                    let trimmed = T.drop indent l
                    T.take 1 trimmed === ","
                ) subsequent
        else success

    it "Property: insertDependencyLine preserves context and adds dependency" $ hedgehog $ do
      -- Setup random context
      prefix <- forAll genRandomText
      
      -- Existing dependencies block
      existingDeps <- forAll genDependencies
      keyIndent <- forAll $ Gen.int (Range.linear 0 4)
      let indentStr = T.replicate keyIndent " "
      let keyLine = indentStr <> "build-depends:"
      
      -- Content indentation must be > keyIndent
      let contentIndent = keyIndent + 2
      let existingBlock = formatDependencyList "\n" True existingDeps contentIndent
      
      -- 'suffix' must break the block (indent <= keyIndent)
      suffix <- forAll $ genAfterText keyIndent
      
      -- Construct full content
      let fullContent = prefix <> "\n" <> keyLine <> existingBlock <> "\n" <> suffix
      
      -- New dependency to insert
      newDep <- forAll genDependency
      
      -- Perform insertion
      let result = insertDependencyLine "\n" True newDep fullContent
      
      -- Verification
      annotateShow fullContent
      annotateShow result
      
      -- 1. Context preservation
      assert $ T.isInfixOf (T.strip prefix) result
      assert $ T.isInfixOf (T.strip suffix) result
      
      -- 2. New dependency exists
      assert $ T.isInfixOf (unPackageName (depName newDep)) result
      success

    it "Property: insertDependencyLine handles empty build-depends" $ hedgehog $ do
      prefix <- forAll genRandomText
      keyIndent <- forAll $ Gen.int (Range.linear 0 4)
      let indentStr = T.replicate keyIndent " "
      
      -- 'suffix' must break the block
      suffix <- forAll $ genAfterText keyIndent

      let fullContent = prefix <> "\n" <> indentStr <> "build-depends:\n" <> suffix
      newDep <- forAll genDependency
      
      let result = insertDependencyLine "\n" True newDep fullContent
      
      annotateShow result
      assert $ T.isInfixOf (unPackageName (depName newDep)) result
      assert $ T.isInfixOf "," result == False

    it "Property: replaceBuildDependsBlock correctly replaces dependencies" $ hedgehog $ do
      prefix <- forAll genRandomText
      
      -- Initial content with some dependencies
      oldDeps <- forAll genDependencies
      keyIndent <- forAll $ Gen.int (Range.linear 0 4)
      let indentStr = T.replicate keyIndent " "
      let keyLine = indentStr <> "build-depends:"
      let contentIndent = keyIndent + 2
      let oldBlock = formatDependencyList "\n" True oldDeps contentIndent
      
      suffix <- forAll $ genAfterText keyIndent
      
      let fullContent = prefix <> "\n" <> keyLine <> oldBlock <> "\n" <> suffix
      
      -- New dependencies to replace with
      newDeps <- forAll genDependencies
      
      let result = replaceBuildDependsBlock "\n" True newDeps fullContent
      
      annotateShow fullContent
      annotateShow result
      
      -- 1. Context preservation
      assert $ T.isInfixOf (T.strip prefix) result
      assert $ T.isInfixOf (T.strip suffix) result
      
      -- 2. New dependencies present
      forM_ newDeps $ \d -> 
        assert $ T.isInfixOf (unPackageName (depName d)) result
      
      success

-- Generators

genDependency :: Gen Dependency
genDependency = do
  name <- genPackageName
  
  ver <- Gen.maybe genVersionConstraint
  return $ Dependency name ver BuildDepends

genDependencies :: Gen [Dependency]
genDependencies = Gen.list (Range.linear 1 10) genDependency

genPackageName :: Gen PackageName
genPackageName = do
  first <- Gen.alpha
  middle <- Gen.text (Range.linear 0 20) (Gen.choice [Gen.alphaNum, pure '-'])
  lastC <- Gen.alphaNum
  let name = T.cons first (middle <> T.singleton lastC)
  if "--" `T.isInfixOf` name
    then genPackageName
    else return $ trustedMkPackageName name

genVersionConstraint :: Gen VersionConstraint
genVersionConstraint = Gen.choice
  [ pure AnyVersion
  , ExactVersion <$> genVersion
  , UnparsedVersion <$> genComplexConstraint
  ]

genVersion :: Gen Version
genVersion = do
  nums <- Gen.list (Range.linear 1 4) (Gen.int (Range.linear 0 100))
  return $ Version nums

genComplexConstraint :: Gen Text
genComplexConstraint = do
  op <- Gen.element [">=", "<", "==", "^>="]
  v <- genVersion
  return $ op <> " " <> showVer v

showVer :: Version -> Text
showVer (Version nums) = T.intercalate "." (map (T.pack . show) nums)

genRandomText :: Gen Text
genRandomText = do
  txtLines <- Gen.list (Range.linear 1 10) genRandomLine
  return $ T.unlines txtLines

genRandomLine :: Gen Text
genRandomLine = do
  indent <- Gen.text (Range.linear 0 8) (Gen.element [' ', '\t'])
  content <- Gen.choice 
    [ Gen.text (Range.linear 1 50) Gen.alphaNum
    , genComment
    ]
  return $ indent <> content

genComment :: Gen Text
genComment = do
  content <- Gen.text (Range.linear 1 50) Gen.alphaNum
  return $ "-- " <> content

genAfterText :: Int -> Gen Text
genAfterText maxIndent = do
  -- First line must be indented <= maxIndent to break the block
  firstIndentLen <- Gen.int (Range.linear 0 maxIndent)
  let firstIndent = T.replicate firstIndentLen " "
  
  firstContent <- Gen.text (Range.linear 1 20) Gen.alphaNum
  let firstLine = firstIndent <> firstContent
  
  -- Subsequent lines can be anything
  rest <- genRandomText
  return $ firstLine <> "\n" <> rest
