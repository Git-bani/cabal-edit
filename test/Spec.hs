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
import qualified Core.DependencyResolverSpec
import qualified Core.ProjectContextSpec
import qualified Core.SafetySpec
import qualified Core.UnicodeSpec
import qualified Golden.RoundtripSpec
import qualified Integration.EndToEndSpec
import qualified Integration.WorkspaceSpec
import qualified Integration.HpackSpec
import qualified Utils.ConfigSpec
import qualified Utils.DiffSpec
import qualified Core.AST.RoundtripSpec
import qualified Core.AST.EditorSpec
import qualified Core.AST.EditorOutlierSpec
import qualified Business.DebugAddSpec
import qualified Business.FlagOutlierSpec
import qualified Business.SetVersionOutlierSpec

main :: IO ()
main = hspec $ do
  describe "Core.AST.Roundtrip" Core.AST.RoundtripSpec.spec
  describe "Core.AST.Editor" Core.AST.EditorSpec.spec
  describe "Core.AST.Editor (Outliers)" Core.AST.EditorOutlierSpec.spec
  describe "Business.Add" Business.AddSpec.spec
  describe "Business.DebugAdd" Business.DebugAddSpec.spec
  describe "Business.Remove" Business.RemoveSpec.spec
  describe "Business.Upgrade" Business.UpgradeSpec.spec
  describe "Business.SubLibrary" Business.SubLibrarySpec.spec
  describe "Business.Validation" Business.ValidationSpec.spec
  describe "Business.SetVersion" Business.SetVersionSpec.spec
  describe "Business.SetVersion (Outliers)" Business.SetVersionOutlierSpec.spec
  describe "Business.Flag" Business.FlagSpec.spec
  describe "Business.Flag (Outliers)" Business.FlagOutlierSpec.spec
  describe "Business.List" Business.ListSpec.spec
  describe "Business.SourceDep" Business.SourceDepSpec.spec
  describe "Business.CommonStanza" Business.CommonStanzaSpec.spec
  describe "Core.DependencyResolver" Core.DependencyResolverSpec.spec
  describe "Core.ProjectContext" Core.ProjectContextSpec.spec
  describe "Core.Safety" Core.SafetySpec.spec
  describe "Core.Unicode" Core.UnicodeSpec.spec
  describe "Golden.Roundtrip" Golden.RoundtripSpec.spec
  describe "Integration.EndToEnd" Integration.EndToEndSpec.spec
  describe "Integration.Workspace" Integration.WorkspaceSpec.spec
  describe "Integration.Hpack" Integration.HpackSpec.spec
  describe "Utils.Config" Utils.ConfigSpec.spec
  describe "Utils.Diff" Utils.DiffSpec.spec
