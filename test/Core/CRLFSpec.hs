{-# LANGUAGE OverloadedStrings #-}

module Core.CRLFSpec (spec) where

import Test.Hspec
import Core.Serializer
import Core.Types
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "CRLF Handling" $ do
    it "does not introduce double CR when inserting dependencies" $ do
      let content = "name: test\r\nbuild-depends:\r\n  base\r\n"
          eol = "\r\n"
          dep = Dependency (trustedMkPackageName "text") Nothing BuildDepends
          
          -- We use insertDependencyLine directly to test the core logic
          result = insertDependencyLine eol True dep content
      
      -- If bug exists, result will have \r\r\n
      T.isInfixOf "\r\r\n" result `shouldBe` False
      
      -- Verify it still looks correct
      T.isInfixOf "text" result `shouldBe` True
      T.isInfixOf "\r\n" result `shouldBe` True

    it "handles updateDependencies correctly with CRLF" $ do
      let content = "name: test\r\nlibrary\r\n  build-depends:\r\n    base\r\n"
          eol = "\r\n"
          pkgName = trustedMkPackageName "test"
          dep = Dependency (trustedMkPackageName "text") Nothing BuildDepends
          
          cf = CabalFile 
               { cfPackageName = pkgName
               , cfSections = [LibrarySection (Library Nothing [dep] (TextSpan 0 0))] -- dummy section
               , cfRawContent = content
               , cfLineEndings = eol
               }
          
          updatedCf = updateDependencies cf True [dep] Add
          result = cfRawContent updatedCf
          
      T.isInfixOf "\r\r\n" result `shouldBe` False
