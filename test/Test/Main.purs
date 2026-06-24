module Test.Main where

import Prelude

import Effect (Effect)
import Test.Model.CodecSpec as CodecSpec
import Test.Model.CommandSpec as CommandSpec
import Test.Model.DropSpec as DropSpec
import Test.Model.GuardSpec as GuardSpec
import Test.Model.GuideSpec as GuideSpec
import Test.Model.PortableImportSpec as PortableImportSpec
import Test.Model.ReconcileSpec as ReconcileSpec
import Test.Model.RematchSpec as RematchSpec
import Test.Model.ScrollSpec as ScrollSpec
import Test.Model.ShortcutsSpec as ShortcutsSpec
import Test.Model.TreeSpec as TreeSpec
import Test.Model.UndoSpec as UndoSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- runSpecAndExitProcess exits the process non-zero on failure, so `spago test`
-- (and `pnpm check`) actually fail when a test fails — required for the
-- no-manual-testing contract.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  TreeSpec.spec
  ReconcileSpec.spec
  CodecSpec.spec
  CommandSpec.spec
  DropSpec.spec
  RematchSpec.spec
  PortableImportSpec.spec
  GuardSpec.spec
  GuideSpec.spec
  ScrollSpec.spec
  ShortcutsSpec.spec
  UndoSpec.spec
