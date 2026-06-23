module Test.Model.ShortcutsSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Model.Shortcuts (Cmd(..), bindingFor, cmdForCombo, formatCombo, toCommandShortcut)
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

  it "toCommandShortcut upper-cases the key and keeps modifiers" do
    toCommandShortcut false "Ctrl+Shift+y" `shouldEqual` Right "Ctrl+Shift+Y"

  it "toCommandShortcut maps Mac modifiers (Meta -> Command, Ctrl -> MacCtrl)" do
    toCommandShortcut true "Meta+Shift+y" `shouldEqual` Right "Command+Shift+Y"
    toCommandShortcut true "Ctrl+y" `shouldEqual` Right "MacCtrl+Y"

  it "toCommandShortcut orders the primary modifier before Shift" do
    toCommandShortcut false "Ctrl+Shift+k" `shouldEqual` Right "Ctrl+Shift+K"

  it "toCommandShortcut maps named keys to the commands vocabulary" do
    toCommandShortcut false "Alt+ArrowUp" `shouldEqual` Right "Alt+Up"
    toCommandShortcut false "Ctrl+." `shouldEqual` Right "Ctrl+Period"

  it "toCommandShortcut rejects a combo with no primary modifier" do
    toCommandShortcut false "n" `shouldEqual` Left "Add a modifier such as Ctrl or Alt — browser shortcuts require one."
    toCommandShortcut false "Shift+n" `shouldEqual` Left "Add a modifier such as Ctrl or Alt — browser shortcuts require one."

  it "toCommandShortcut rejects keys the commands API can't express" do
    toCommandShortcut false "Ctrl+/" `shouldEqual` Left "That key can't be used for a browser shortcut."
