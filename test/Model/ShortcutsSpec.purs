module Test.Model.ShortcutsSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Model.Shortcuts (Cmd(..), bindingFor, cmdForCombo, formatCombo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Model.Shortcuts" do
  it "falls back to the default when there is no override" do
    bindingFor Object.empty NewGroup `shouldEqual` "n"

  it "an override wins over the default" do
    bindingFor (Object.singleton "newGroup" "Ctrl+Shift+g") NewGroup `shouldEqual` "Ctrl+Shift+g"

  it "an empty override falls back to the default" do
    bindingFor (Object.singleton "newGroup" "") NewGroup `shouldEqual` "n"

  it "matches a pressed combo to its command (defaults)" do
    cmdForCombo Object.empty "/" `shouldEqual` Just FocusSearch
    cmdForCombo Object.empty "n" `shouldEqual` Just NewGroup

  it "an unbound combo matches nothing" do
    cmdForCombo Object.empty "q" `shouldEqual` Nothing

  it "matching honors overrides — the new combo fires, the old default doesn't" do
    let o = Object.singleton "newGroup" "g"
    cmdForCombo o "g" `shouldEqual` Just NewGroup
    cmdForCombo o "n" `shouldEqual` Nothing

  it "formatCombo upper-cases a trailing single letter only" do
    formatCombo "shift+n" `shouldEqual` "shift+N"
    formatCombo "n" `shouldEqual` "N"
    formatCombo "/" `shouldEqual` "/"
