{-# LANGUAGE OverloadedStrings #-}

module Business.ListSpec (spec) where

import Test.Hspec
import Business.List (formatDependencies)
import Core.Types
import qualified Data.Text as T

spec :: Spec
spec = describe "Business.List" $ do
  
  describe "formatDependencies" $ do
    it "lists dependencies for a simple library" $ do
      let cabalFile = simpleCabalFile [baseDep, textDep]
      let output = formatDependencies (ListOptions False) cabalFile
      
      output `shouldSatisfy` ("Dependencies for test-pkg" `T.isInfixOf`)
      output `shouldSatisfy` ("Library:" `T.isInfixOf`)
      output `shouldSatisfy` ("  - base >=4.14" `T.isInfixOf`)
      output `shouldSatisfy` ("  - text >=1.2" `T.isInfixOf`)

    it "lists dependencies for multiple sections" $ do
      let cabalFile = multiSectionCabalFile
      let output = formatDependencies (ListOptions False) cabalFile
      
      output `shouldSatisfy` ("Library:" `T.isInfixOf`)
      output `shouldSatisfy` ("Executable my-exe:" `T.isInfixOf`)
      output `shouldSatisfy` ("Test Suite my-test:" `T.isInfixOf`)
      output `shouldSatisfy` ("Benchmark my-bench:" `T.isInfixOf`)
      
      -- Verify dependencies in correct sections
      let lines' = T.lines output
      
      -- Simple check: ensure sections appear
      length (filter ("Library:" `T.isInfixOf`) lines') `shouldBe` 1
      length (filter ("Executable my-exe:" `T.isInfixOf`) lines') `shouldBe` 1

    it "handles empty dependencies gracefully" $ do
      let cabalFile = simpleCabalFile []
      let output = formatDependencies (ListOptions False) cabalFile
      
      output `shouldSatisfy` ("Dependencies for test-pkg" `T.isInfixOf`)
      output `shouldSatisfy` (not . ("Library:" `T.isInfixOf`)) -- Should skip empty sections
      output `shouldSatisfy` (not . ("- base" `T.isInfixOf`))

    it "sorts dependencies alphabetically" $ do
      let cabalFile = simpleCabalFile [textDep, baseDep] -- Unsorted input
      let output = formatDependencies (ListOptions False) cabalFile
      
      let lines' = filter ("  - " `T.isPrefixOf`) (T.lines output)
      lines' `shouldBe` ["  - base >=4.14", "  - text >=1.2"]

    it "formats various version constraints correctly" $ do
      let deps = [ baseDep
                 , dep "exact" (ExactVersion (Version [1,0]))
                 , dep "major" (MajorBoundVersion (Version [2,1]))
                 , dep "none" AnyVersion
                 , dep "unparsed" (UnparsedVersion ">= 1.2.3 && < 2")
                 ]
      let cabalFile = simpleCabalFile deps
      let output = formatDependencies (ListOptions False) cabalFile
      
      output `shouldSatisfy` ("base >=4.14" `T.isInfixOf`)
      output `shouldSatisfy` ("exact ==1.0" `T.isInfixOf`)
      output `shouldSatisfy` ("major ^>=2.1" `T.isInfixOf`)
      output `shouldSatisfy` ("none (*)" `T.isInfixOf`)
      output `shouldSatisfy` ("unparsed >= 1.2.3 && < 2" `T.isInfixOf`)

-- Helpers

simpleCabalFile :: [Dependency] -> CabalFile
simpleCabalFile deps = CabalFile
  {
 cfPackageName = trustedMkPackageName "test-pkg"
  , cfSections = [LibrarySection (Library Nothing deps (TextSpan 0 0))]
  , cfRawContent = ""
  , cfLineEndings = "\n"
  }

multiSectionCabalFile :: CabalFile
multiSectionCabalFile = CabalFile
  {
 cfPackageName = trustedMkPackageName "complex-pkg"
  , cfSections = 
      [
 LibrarySection (Library Nothing [baseDep] (TextSpan 0 0))
      , ExecutableSection (Executable "my-exe" [textDep] (TextSpan 0 0))
      , TestSuiteSection (TestSuite "my-test" [baseDep] (TextSpan 0 0))
      , BenchmarkSection (Benchmark "my-bench" [baseDep] (TextSpan 0 0))
      ]
  , cfRawContent = ""
  , cfLineEndings = "\n"
  }

baseDep :: Dependency
baseDep = Dependency (trustedMkPackageName "base") (Just (UnparsedVersion ">=4.14")) BuildDepends

textDep :: Dependency
textDep = Dependency (trustedMkPackageName "text") (Just (UnparsedVersion ">=1.2")) BuildDepends

dep :: T.Text -> VersionConstraint -> Dependency
dep name vc = Dependency (trustedMkPackageName name) (Just vc) BuildDepends
