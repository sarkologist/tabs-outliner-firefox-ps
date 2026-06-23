module Test.Main where

import Prelude

import Effect (Effect)
import Test.Model.CommandSpec as CommandSpec
import Test.Model.GuardSpec as GuardSpec
import Test.Model.PortableImportSpec as PortableImportSpec
import Test.Model.ReconcileSpec as ReconcileSpec
import Test.Model.RematchSpec as RematchSpec
import Test.Model.TreeSpec as TreeSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- runSpecAndExitProcess exits the process non-zero on failure, so `spago test`
-- (and `pnpm check`) actually fail when a test fails — required for the
-- no-manual-testing contract.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  TreeSpec.spec
  ReconcileSpec.spec
  CommandSpec.spec
  RematchSpec.spec
  PortableImportSpec.spec
  GuardSpec.spec
