{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Business.AddSpec
import qualified Business.RemoveSpec
import qualified Business.UpgradeSpec
import qualified Business.SubLibrarySpec
import qualified Business.ValidationSpec
import qualified Business.SetVersionSpec
import qualified Business.FlagSpec
import qualified Business.ListSpec
import qualified Business.SourceDepSpec
import qualified Business.CommonStanzaSpec
import qualified Core.CRLFSpec
import qualified Core.DependencyResolverSpec
import qualified Core.ParserSpec
import qualified Core.ProjectContextSpec
import qualified Core.SafetySpec
import qualified Core.SerializerComplexSpec
import qualified Core.SerializerOutlierSpec
import qualified Core.SerializerSpec
import qualified Core.TrailingCommaSpec
import qualified Core.UnicodeSpec
import qualified Golden.RoundtripSpec
import qualified Integration.EndToEndSpec
import qualified Integration.WorkspaceSpec
import qualified Integration.HpackSpec
import qualified Utils.ConfigSpec
import qualified Utils.DiffSpec

main :: IO ()
main = hspec $ do
  describe "Business.Add" Business.AddSpec.spec
  describe "Business.Remove" Business.RemoveSpec.spec
  describe "Business.Upgrade" Business.UpgradeSpec.spec
  describe "Business.SubLibrary" Business.SubLibrarySpec.spec
  describe "Business.Validation" Business.ValidationSpec.spec
  describe "Business.SetVersion" Business.SetVersionSpec.spec
  describe "Business.Flag" Business.FlagSpec.spec
  describe "Business.List" Business.ListSpec.spec
  describe "Business.SourceDep" Business.SourceDepSpec.spec
  describe "Business.CommonStanza" Business.CommonStanzaSpec.spec
  describe "Core.CRLF" Core.CRLFSpec.spec
  describe "Core.DependencyResolver" Core.DependencyResolverSpec.spec
  describe "Core.Parser" Core.ParserSpec.spec
  describe "Core.ProjectContext" Core.ProjectContextSpec.spec
  describe "Core.Safety" Core.SafetySpec.spec
  describe "Core.SerializerComplex" Core.SerializerComplexSpec.spec
  describe "Core.SerializerOutlier" Core.SerializerOutlierSpec.spec
  describe "Core.Serializer" Core.SerializerSpec.spec
  describe "Core.TrailingComma" Core.TrailingCommaSpec.spec
  describe "Core.Unicode" Core.UnicodeSpec.spec
  describe "Golden.Roundtrip" Golden.RoundtripSpec.spec
  describe "Integration.EndToEnd" Integration.EndToEndSpec.spec
  describe "Integration.Workspace" Integration.WorkspaceSpec.spec
  describe "Integration.Hpack" Integration.HpackSpec.spec
  describe "Utils.Config" Utils.ConfigSpec.spec
  describe "Utils.Diff" Utils.DiffSpec.spec
