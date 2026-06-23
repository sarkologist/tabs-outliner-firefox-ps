module Test.Model.UndoSpec where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (Command(..), applyCommand)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Tree (applyPatch)
import Model.Types (Kind(..), Model, Status(..), defaultNode, emptyModel)
import Model.Undo (applyEntry, inversePatch, undoable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (\m e -> (applyBrowser 0.0 e m).model) emptyModel

-- window n1 with live tabs n2 (tab 11, "A") and n3 (tab 12, "B")
base :: Model
base = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false ]

-- apply a command, then undo it: apply the inverse (computed against the
-- pre-command model) to the post-command model.
undone :: Command -> Model -> Model
undone c m =
  let r = applyCommand 0.0 c m
  in applyPatch (inversePatch 0.0 m r.patch) r.model

spec :: Spec Unit
spec = describe "Model.Undo" do
  it "marks structural edits undoable; view toggles and browser actions are not" do
    undoable (Collapse "n1" true) `shouldEqual` false
    undoable (Activate "n1") `shouldEqual` false
    undoable (CloseNode "n1") `shouldEqual` false
    undoable (Rename "n1" "x") `shouldEqual` true
    undoable (Delete "n1") `shouldEqual` true
    undoable (Move "n1" Nothing 0) `shouldEqual` true
    undoable (Flatten "n1") `shouldEqual` true
    undoable (NewGroup Nothing 0) `shouldEqual` true
    undoable (Import { nodes: [], roots: [] }) `shouldEqual` true

  it "undo of rename restores the prior (absent) custom title" do
    let back = undone (Rename "n2" "X") base
    (_.customTitle <$> Map.lookup "n2" back.nodes) `shouldEqual` Just Nothing

  it "undo of move restores parent, sibling order, and roots" do
    let back = undone (Move "n3" Nothing 0) base
    (_.parent <$> Map.lookup "n3" back.nodes) `shouldEqual` Just (Just "n1")
    (_.children <$> Map.lookup "n1" back.nodes) `shouldEqual` Just [ "n2", "n3" ]
    back.roots `shouldEqual` [ "n1" ]

  it "undo of new group removes the created node and restores roots" do
    let back = undone (NewGroup Nothing 0) base
    Map.member "n4" back.nodes `shouldEqual` false
    back.roots `shouldEqual` [ "n1" ]

  it "undo of flatten restores the dissolved group and its children" do
    let
      m1 = (applyCommand 0.0 (NewGroup Nothing 0) base).model -- group n4 at roots[0]
      m2 = (applyCommand 0.0 (Move "n1" (Just "n4") 0) m1).model -- window n1 under the group
      back = undone (Flatten "n4") m2
    (_.kind <$> Map.lookup "n4" back.nodes) `shouldEqual` Just KGroup
    (_.children <$> Map.lookup "n4" back.nodes) `shouldEqual` Just [ "n1" ]
    back.roots `shouldEqual` [ "n4" ]

  it "undo of delete brings the subtree back as closed history (browser objects are gone)" do
    let back = undone (Delete "n1") base
    Map.member "n1" back.nodes `shouldEqual` true
    Map.member "n2" back.nodes `shouldEqual` true
    Map.member "n3" back.nodes `shouldEqual` true
    -- the previously-live window/tabs return Closed (their tabs were closed), and
    -- their browser bindings are cleared, but the tree structure is intact
    (_.status <$> Map.lookup "n1" back.nodes) `shouldEqual` Just Closed
    (_.status <$> Map.lookup "n2" back.nodes) `shouldEqual` Just Closed
    (_.tabId <$> Map.lookup "n2" back.nodes) `shouldEqual` Just Nothing
    (_.children <$> Map.lookup "n1" back.nodes) `shouldEqual` Just [ "n2", "n3" ]
    back.roots `shouldEqual` [ "n1" ]

  it "undo of deleting one live tab returns it as closed history under its window" do
    let back = undone (Delete "n2") base
    (_.status <$> Map.lookup "n2" back.nodes) `shouldEqual` Just Closed
    (_.children <$> Map.lookup "n1" back.nodes) `shouldEqual` Just [ "n2", "n3" ]

  it "undo of import removes the imported nodes" do
    let
      snap = { nodes: [ (defaultNode "g1" KGroup 0.0) { title = "G" } ], roots: [ "g1" ] }
      back = undone (Import snap) base
    -- base.nextId is 4, so the import allocates n4; undo drops it
    Map.member "n4" back.nodes `shouldEqual` false
    back.roots `shouldEqual` [ "n1" ]

  it "redo re-applies exactly what undo reverted" do
    let
      r = applyCommand 0.0 (Move "n3" Nothing 0) base
      undoStep = applyEntry 0.0 (inversePatch 0.0 base r.patch) r.model
      redoStep = applyEntry 0.0 undoStep.inverse undoStep.model
    -- undo put n3 back under n1...
    (_.parent <$> Map.lookup "n3" undoStep.model.nodes) `shouldEqual` Just (Just "n1")
    -- ...and redo moves it back to the root, matching the original command
    (_.parent <$> Map.lookup "n3" redoStep.model.nodes) `shouldEqual` Just Nothing
    redoStep.model.roots `shouldEqual` [ "n3", "n1" ]

  it "undo does not drop a window that opened since the command" do
    let
      m0 = (applyCommand 0.0 (NewGroup Nothing 0) base).model -- roots [n4, n1]
      del = applyCommand 0.0 (Delete "n4") m0 -- roots [n1]
      entry = inversePatch 0.0 m0 del.patch -- roots Just [n4, n1]
      -- a browser window opens after the delete, appending a fresh root (n5)
      withWin = (applyBrowser 0.0 (WindowOpened { windowId: 2 }) del.model).model
      undoStep = applyEntry 0.0 entry withWin
    -- the restored group, the kept window, AND the new window all survive
    undoStep.model.roots `shouldEqual` [ "n4", "n1", "n5" ]
    Map.member "n5" undoStep.model.nodes `shouldEqual` true
