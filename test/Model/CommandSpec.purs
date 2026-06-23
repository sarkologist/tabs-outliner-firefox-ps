module Test.Model.CommandSpec where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (BrowserAction(..), Command(..), applyCommand)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Types (Kind(..), Model, Status(..), defaultNode, emptyModel)
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

run :: Command -> Model -> Model
run c m = (applyCommand 0.0 c m).model

spec :: Spec Unit
spec = describe "Model.Command" do
  it "collapse sets the flag" do
    (_.collapsed <$> Map.lookup "n1" (run (Collapse "n1" true) base).nodes) `shouldEqual` Just true

  it "rename sets a custom title" do
    (_.customTitle <$> Map.lookup "n2" (run (Rename "n2" "X") base).nodes) `shouldEqual` Just (Just "X")

  it "activate a live tab focuses it" do
    (applyCommand 0.0 (Activate "n2") base).actions `shouldEqual` [ FocusTab 11 ]

  it "close a window closes all its live tabs" do
    (applyCommand 0.0 (CloseNode "n1") base).actions `shouldEqual` [ RemoveTab 11, RemoveTab 12 ]

  it "delete removes the subtree and closes its live tabs" do
    let r = applyCommand 0.0 (Delete "n2") base
    Map.lookup "n2" r.model.nodes `shouldEqual` Nothing
    (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "n3" ]
    r.actions `shouldEqual` [ RemoveTab 11 ]

  it "move re-parents a node to the root" do
    let m = run (Move "n3" Nothing 0) base
    (_.parent <$> Map.lookup "n3" m.nodes) `shouldEqual` Just Nothing
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2" ]
    m.roots `shouldEqual` [ "n3", "n1" ]

  it "new group then flatten promotes children and removes the group" do
    let
      m1 = run (NewGroup Nothing 0) base -- group n4 at roots[0]
      m2 = run (Move "n2" (Just "n4") 0) m1 -- A under the group
      m3 = run (Flatten "n4") m2
    (_.kind <$> Map.lookup "n4" m1.nodes) `shouldEqual` Just KGroup
    Map.lookup "n4" m3.nodes `shouldEqual` Nothing
    m3.roots `shouldEqual` [ "n2", "n1" ]
    (_.parent <$> Map.lookup "n2" m3.nodes) `shouldEqual` Just Nothing

  it "move into one's own descendant is rejected (no cycle)" do
    let m = run (Move "n1" (Just "n2") 0) base -- n2 is a child of n1
    (_.parent <$> Map.lookup "n1" m.nodes) `shouldEqual` Just Nothing

  it "flatten does nothing on a non-group" do
    let m = run (Flatten "n2") base -- n2 is a tab
    (_.kind <$> Map.lookup "n2" m.nodes) `shouldEqual` Just KTab
    Map.size m.nodes `shouldEqual` Map.size base.nodes

  it "closing a window keeps nested groups live (groups have no browser state)" do
    let
      withGroup = run (NewGroup (Just "n1") 0) base -- group n4 under window n1
      closed = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) withGroup).model
    (_.status <$> Map.lookup "n1" closed.nodes) `shouldEqual` Just Closed
    (_.status <$> Map.lookup "n4" closed.nodes) `shouldEqual` Just Live

  it "import adds an exported outline as inert, restorable top-level history" do
    let
      grp = (defaultNode "g1" KGroup 0.0) { title = "G", children = [ "t1" ] }
      tab = (defaultNode "t1" KTab 0.0) { title = "T", url = Just "http://t", tabId = Just 5, parent = Just "g1" }
      r = applyCommand 0.0 (Import { nodes: [ grp, tab ], roots: [ "g1" ] }) base
    -- fresh ids (base.nextId is 4): g1 -> n4, t1 -> n5; appended to roots
    r.model.roots `shouldEqual` [ "n1", "n4" ]
    (_.kind <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just KGroup
    (_.status <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just Live -- groups stay live
    (_.children <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just [ "n5" ]
    (_.status <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just Closed -- imported tab is inert
    (_.tabId <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just Nothing
    (_.parent <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just (Just "n4")

  it "restore re-binds the existing node when the tab re-opens (no duplicate)" do
    let
      closed = runEvents [ openTab 11 1 0 "A" true, TabClosed { tabId: 11 } ]
      activated = applyCommand 0.0 (Activate "n2") closed
      reopened = (applyBrowser 0.0 (openTab 99 1 0 "A" true) activated.model).model
    activated.actions `shouldEqual` [ CreateTab Nothing (Just "http://A") ]
    -- same node id, now live and bound to the new tab; no extra node created
    (_.status <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just Live
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 99)
    Map.size reopened.nodes `shouldEqual` 2
