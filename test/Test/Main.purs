module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- runSpecAndExitProcess exits the process non-zero on failure, so `spago test`
-- (and `pnpm check`) actually fail when a test fails — required for the
-- no-manual-testing contract.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "sanity" do
    it "the toolchain runs a test" do
      (1 + 1) `shouldEqual` 2
