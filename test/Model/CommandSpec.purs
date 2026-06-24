module Test.Model.CommandSpec where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (BrowserAction(..), Command(..), applyCommand)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Types (Kind(..), Model, defaultNode, emptyModel, isLive)
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

  it "flatten leaves a live window intact (PR1 gate: would orphan its live tabs)" do
    let m = run (Flatten "n1") base -- n1 is the live window (windowId 1)
    (_.kind <$> Map.lookup "n1" m.nodes) `shouldEqual` Just KGroup
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3" ]
    Map.size m.nodes `shouldEqual` Map.size base.nodes

  it "closing a window drops its window binding but leaves nested groups untouched" do
    let
      withGroup = run (NewGroup (Just "n1") 0) base -- group n4 under window n1
      closed = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) withGroup).model
    -- the window container is no longer live: its windowId binding is gone, so it
    -- now reads as a plain saved group
    (isLive <$> Map.lookup "n1" closed.nodes) `shouldEqual` Just false
    (_.windowId <$> Map.lookup "n1" closed.nodes) `shouldEqual` Just Nothing
    -- the nested user group never had a browser binding, so close leaves it untouched
    (_.kind <$> Map.lookup "n4" closed.nodes) `shouldEqual` Just KGroup
    (_.closedAt <$> Map.lookup "n4" closed.nodes) `shouldEqual` Just Nothing

  it "import adds an exported outline as inert, restorable top-level history" do
    let
      grp = (defaultNode "g1" KGroup 0.0) { title = "G", children = [ "t1" ] }
      tab = (defaultNode "t1" KTab 0.0) { title = "T", url = Just "http://t", tabId = Just 5, parent = Just "g1" }
      r = applyCommand 0.0 (Import { nodes: [ grp, tab ], roots: [ "g1" ] }) base
    -- fresh ids (base.nextId is 4): g1 -> n4, t1 -> n5; appended to roots
    r.model.roots `shouldEqual` [ "n1", "n4" ]
    (_.kind <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just KGroup
    -- every imported node is inert (no browser binding): the container is a plain
    -- saved group, the tab restorable history (keeps its url, drops its tabId)
    (isLive <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just false
    (_.children <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just [ "n5" ]
    (isLive <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just false
    (_.tabId <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just Nothing
    (_.url <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just (Just "http://t")
    (_.parent <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just (Just "n4")

  it "restore re-binds the existing node when the tab re-opens (no duplicate)" do
    let
      closed = runEvents [ openTab 11 1 0 "A" true, TabClosed { tabId: 11 } ]
      activated = applyCommand 0.0 (Activate "n2") closed
      reopened = (applyBrowser 0.0 (openTab 99 1 0 "A" true) activated.model).model
    -- the window is still live, so the tab reopens back into it (not a new window)
    activated.actions `shouldEqual` [ CreateTab (Just 1) (Just "http://A") ]
    -- same node id, now live and bound to the new tab; no extra node created
    (isLive <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just true
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 99)
    Map.size reopened.nodes `shouldEqual` 2

  it "restoring a closed window opens a new window (not the active one)" do
    let
      closedWin = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) base).model
      activated = applyCommand 0.0 (Activate "n1") closedWin
    -- one new window carrying both tabs' urls, in order — no bare CreateTab
    activated.actions `shouldEqual` [ CreateWindow [ "http://A", "http://B" ] ]
    -- the closed window node is queued to rebind to the window that opens
    activated.model.pendingRestoreWindows `shouldEqual` [ "n1" ]

  it "the restored window node goes live in place when its window opens (no duplicate)" do
    let
      closedWin = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) base).model
      activated = applyCommand 0.0 (Activate "n1") closedWin
      -- the browser opens the new window (id 2) and re-creates both tabs in it
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 2 }
        , openTab 21 2 0 "A" true
        , openTab 22 2 1 "B" false
        ]
    -- the existing window node n1 is now live and bound to the new browser window
    (isLive <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just true
    (_.windowId <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just (Just 2)
    reopened.pendingRestoreWindows `shouldEqual` []
    -- its tabs re-bound to their existing nodes, still under n1, all live
    (isLive <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just true
    (isLive <$> Map.lookup "n3" reopened.nodes) `shouldEqual` Just true
    (_.children <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just [ "n2", "n3" ]
    -- no phantom window node, no duplicated tabs
    reopened.roots `shouldEqual` [ "n1" ]
    Map.size reopened.nodes `shouldEqual` 3

  -- The unification: a saved GROUP (never a browser window) restores exactly like
  -- a saved window — its owning container goes live in place. A node's owning
  -- window is its immediate parent, so restoring the tab lights up that parent.
  it "restoring a saved group lights it up as a new window in place (group goes live)" do
    let
      grp = (defaultNode "g1" KGroup 0.0) { title = "Saved", children = [ "t1" ] }
      tab = (defaultNode "t1" KTab 0.0) { title = "T", url = Just "http://t", parent = Just "g1" }
      -- base.nextId is 4, so the imported group -> n4 and its tab -> n5
      saved = (applyCommand 0.0 (Import { nodes: [ grp, tab ], roots: [ "g1" ] }) base).model
      activated = applyCommand 0.0 (Activate "n4") saved
    -- a saved group is not a live window, so its tabs open as ONE new window
    -- (no bare CreateTab into the focused window)
    activated.actions `shouldEqual` [ CreateWindow [ "http://t" ] ]
    -- ...and the group node itself queues to bind that window — it goes live in place
    activated.model.pendingRestoreWindows `shouldEqual` [ "n4" ]
    let
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 5 }, openTab 71 5 0 "t" true ]
    -- the very same group node n4 is now a live window bound to browser window 5
    (isLive <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just true
    (_.windowId <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just (Just 5)
    -- its tab re-bound under it; no duplicate window node appeared
    (isLive <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just true
    (_.parent <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just (Just "n4")
    reopened.roots `shouldEqual` [ "n1", "n4" ]
