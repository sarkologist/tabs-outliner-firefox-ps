module Test.Model.CommandSpec where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (BrowserAction(..), Command(..), applyCommand)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Tree (applyPatch)
import Model.Types (Kind(..), Model, defaultNode, emptyModel, isLive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

-- a TabOpened with an explicit url, to model the browser reporting a different url
-- for a recreated tab than the one stored
openTabU :: Int -> Int -> Int -> String -> String -> BrowserEvent
openTabU tabId windowId index url title =
  TabOpened { tabId, windowId, index, url: Just url, title, active: false, favIconUrl: Nothing }

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (\m e -> (applyBrowser 0.0 e m).model) emptyModel

-- window n1 with live tabs n2 (tab 11, "A") and n3 (tab 12, "B")
base :: Model
base = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false ]

-- base plus a second window n4 (id 2) holding a live tab n5 (tab 21, "C")
base2 :: Model
base2 = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false, openTab 21 2 0 "C" true ]

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

  it "move re-parents a (non-live) node to the root" do
    let
      m0 = run (NewGroup (Just "n1") 0) base -- group n4 as n1's first child
      m = run (Move "n4" Nothing 0) m0
    (_.parent <$> Map.lookup "n4" m.nodes) `shouldEqual` Just Nothing
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3" ]
    m.roots `shouldEqual` [ "n4", "n1" ]

  it "new group then flatten promotes children and removes the group" do
    let
      m1 = run (NewGroup Nothing 0) base -- group n4 at roots[0]
      m2 = run (Move "n1" (Just "n4") 0) m1 -- window n1 under the group (a non-live move)
      m3 = run (Flatten "n4") m2
    (_.kind <$> Map.lookup "n4" m1.nodes) `shouldEqual` Just KGroup
    Map.lookup "n4" m3.nodes `shouldEqual` Nothing
    m3.roots `shouldEqual` [ "n1" ]
    (_.parent <$> Map.lookup "n1" m3.nodes) `shouldEqual` Just Nothing

  it "move into one's own descendant is rejected (no cycle)" do
    let m = run (Move "n1" (Just "n2") 0) base -- n2 is a child of n1
    (_.parent <$> Map.lookup "n1" m.nodes) `shouldEqual` Just Nothing

  it "flatten does nothing on a non-group" do
    let m = run (Flatten "n2") base -- n2 is a tab
    (_.kind <$> Map.lookup "n2" m.nodes) `shouldEqual` Just KTab
    Map.size m.nodes `shouldEqual` Map.size base.nodes

  it "flatten of a live window detaches its tabs into a new window and dissolves it" do
    let r = applyCommand 0.0 (Flatten "n1") base -- n1 is the live window (windowId 1) at root
    Map.lookup "n1" r.model.nodes `shouldEqual` Nothing -- the window node is gone...
    r.actions `shouldEqual` [ NewWindowWithTabs [ 11, 12 ] ] -- ...its tabs re-homed into one fresh window

  it "flatten of a nested live window merges its tabs into the parent window" do
    let
      -- outer window P (id 2) holding its own tab pp and a nested window W (id 1)
      m = applyPatch
        { upserts:
            [ (defaultNode "P" KGroup 0.0) { windowId = Just 2, title = "Outer", children = [ "pp", "W" ] }
            , (defaultNode "pp" KTab 0.0) { parent = Just "P", tabId = Just 20, url = Just "http://p", title = "P0" }
            , (defaultNode "W" KGroup 0.0) { windowId = Just 1, parent = Just "P", title = "Inner", children = [ "t1", "t2" ] }
            , (defaultNode "t1" KTab 0.0) { parent = Just "W", tabId = Just 11, url = Just "http://a", title = "A" }
            , (defaultNode "t2" KTab 0.0) { parent = Just "W", tabId = Just 12, url = Just "http://b", title = "B" }
            ]
        , removes: []
        , roots: Just [ "P" ]
        }
        emptyModel
      r = applyCommand 0.0 (Flatten "W") m
    Map.lookup "W" r.model.nodes `shouldEqual` Nothing -- inner window dissolved
    r.actions `shouldEqual` [ MoveTabToWindow 11 2 (-1), MoveTabToWindow 12 2 (-1) ] -- merged (appended) into the outer window

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

  it "restoring rebinds the clicked node even when the recreated tab reports a different url" do
    let
      closed = runEvents [ openTab 11 1 0 "A" true, TabClosed { tabId: 11 } ]
      activated = applyCommand 0.0 (Activate "n2") closed
      -- the browser recreates the tab, but onCreated reports a normalized/redirected
      -- url ("http://A/" with a trailing slash, not the stored "http://A")
      reopened = (applyBrowser 0.0
        (TabOpened { tabId: 99, windowId: 1, index: 0, url: Just "http://A/", title: "A", active: true, favIconUrl: Nothing })
        activated.model).model
    -- the SAME node n2 is rebound — no duplicate fresh node
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
  it "restoring a closed window with a nested group binds each tab to its own node" do
    let
      -- closed window n1 = [ A(n2), group n3 = [ B(n4) ], C(n5) ] — all closed, with urls
      m0 = applyPatch
        { upserts:
            [ (defaultNode "n1" KGroup 0.0) { title = "W", children = [ "n2", "n3", "n5" ] }
            , (defaultNode "n2" KTab 0.0) { parent = Just "n1", url = Just "http://a", title = "A" }
            , (defaultNode "n3" KGroup 0.0) { parent = Just "n1", title = "G", children = [ "n4" ] }
            , (defaultNode "n4" KTab 0.0) { parent = Just "n3", url = Just "http://b", title = "B" }
            , (defaultNode "n5" KTab 0.0) { parent = Just "n1", url = Just "http://c", title = "C" }
            ]
        , removes: []
        , roots: Just [ "n1" ]
        }
        emptyModel
      activated = applyCommand 0.0 (Activate "n1") m0
      -- window 5 reopens n1's own tabs (A, C); window 6 reopens the group's tab (B).
      -- the recreated tabs report redirected urls, so only window+order matching works.
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 5 }
        , openTabU 51 5 0 "http://a?x" "A"
        , openTabU 52 5 1 "http://c?x" "C"
        , WindowOpened { windowId: 6 }
        , openTabU 61 6 0 "http://b?x" "B"
        ]
    -- each closed node rebinds to its OWN recreated tab — C is not crossed with B
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 51) -- A
    (_.tabId <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just (Just 52) -- C
    (_.tabId <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just (Just 61) -- B, in the group's window
    (_.windowId <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just (Just 5)
    (_.windowId <$> Map.lookup "n3" reopened.nodes) `shouldEqual` Just (Just 6)

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

  -- Dragging a LIVE tab to a new owning container drives the real browser tab; the
  -- tree is left untouched and re-settles from the resulting onAttached/onCreated.
  describe "live-tab moves drive the browser" do
    it "into another live window: moves the real tab there at the dropped index" do
      let r = applyCommand 0.0 (Move "n2" (Just "n4") 1) base2 -- n2 (tab 11) -> window n4 (id 2), index 1
      r.actions `shouldEqual` [ MoveTabToWindow 11 2 1 ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1") -- unchanged until events
      r.model.pendingRestoreWindows `shouldEqual` []

    it "into a saved group: the group goes live as a new window (queued to rebind)" do
      let
        withGroup = (applyCommand 0.0 (NewGroup Nothing 0) base2).model -- group n6 at root
        r = applyCommand 0.0 (Move "n2" (Just "n6") 0) withGroup
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      r.model.pendingRestoreWindows `shouldEqual` [ "n6" ] -- n6 binds when its window opens
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1")

    it "out to the root: detaches into a brand-new window" do
      let r = applyCommand 0.0 (Move "n2" Nothing 0) base2
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      r.model.pendingRestoreWindows `shouldEqual` [] -- a fresh window node appears via onCreated
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1")

    it "within its own window: stays a tree-only reorder (no browser action)" do
      let r = applyCommand 0.0 (Move "n3" (Just "n1") 0) base2
      r.actions `shouldEqual` []
      (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "n3", "n2" ]

  -- "Move to top level" pulls a nested node out to the root just after the root it
  -- belongs to; "Move to bottom" sends it to the very end. A non-live node moves
  -- purely in the tree; a live tab is promoted into its own new window.
  describe "move to top level / bottom" do
    -- two saved top-level groups: R1 = [ A, G=[B] ] and R2 = [ C ] — all closed
    let
      closed = applyPatch
        { upserts:
            [ (defaultNode "R1" KGroup 0.0) { title = "R1", children = [ "A", "G" ] }
            , (defaultNode "A" KTab 0.0) { parent = Just "R1", url = Just "http://a", title = "A" }
            , (defaultNode "G" KGroup 0.0) { parent = Just "R1", title = "G", children = [ "B" ] }
            , (defaultNode "B" KTab 0.0) { parent = Just "G", url = Just "http://b", title = "B" }
            , (defaultNode "R2" KGroup 0.0) { title = "R2", children = [ "C" ] }
            , (defaultNode "C" KTab 0.0) { parent = Just "R2", url = Just "http://c", title = "C" }
            ]
        , removes: []
        , roots: Just [ "R1", "R2" ]
        }
        emptyModel

    it "move to top level pulls a nested node out, just after its root ancestor" do
      let r = applyCommand 0.0 (MoveTopLevel "B") closed
      -- B lands at root index 1 (right after R1), not at the very end
      r.model.roots `shouldEqual` [ "R1", "B", "R2" ]
      (_.parent <$> Map.lookup "B" r.model.nodes) `shouldEqual` Just Nothing
      -- pulling out G's only child prunes the now-empty group; R1 keeps its other child
      Map.lookup "G" r.model.nodes `shouldEqual` Nothing
      (_.children <$> Map.lookup "R1" r.model.nodes) `shouldEqual` Just [ "A" ]
      r.actions `shouldEqual` [] -- tree-only, never touches the browser

    it "move to bottom pulls a nested node to the very end of the root list" do
      let r = applyCommand 0.0 (MoveBottom "B") closed
      r.model.roots `shouldEqual` [ "R1", "R2", "B" ]
      (_.parent <$> Map.lookup "B" r.model.nodes) `shouldEqual` Just Nothing
      Map.lookup "G" r.model.nodes `shouldEqual` Nothing
      r.actions `shouldEqual` []

    it "move to bottom reorders a non-last top-level node to the end" do
      let m = run (MoveBottom "R1") closed
      m.roots `shouldEqual` [ "R2", "R1" ]

    it "move to top level is a no-op on a node already at the top level" do
      (run (MoveTopLevel "R1") closed).roots `shouldEqual` [ "R1", "R2" ]

    it "move to bottom is a no-op on the last top-level node" do
      (run (MoveBottom "R2") closed).roots `shouldEqual` [ "R1", "R2" ]

    -- A live tab can't sit bare at the root, so promoting one detaches the REAL tab
    -- into its own new window (exactly like dragging it to the root); the tree is
    -- left untouched until the resulting browser events arrive.
    it "move to top level on a live tab promotes it into its own new window" do
      let r = applyCommand 0.0 (MoveTopLevel "n2") base -- n2 is a live tab (tab 11) in window n1
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1") -- unchanged until events

    it "move to bottom on a live tab promotes it into its own new window" do
      let r = applyCommand 0.0 (MoveBottom "n2") base
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1")

  -- An emptied container is clutter, so it's pruned — unless the user renamed it,
  -- which marks it as a deliberate label worth keeping.
  describe "pruning emptied groups" do
    -- group n4 at root containing a child group n5 (base.nextId is 4)
    let nested = run (NewGroup (Just "n4") 0) (run (NewGroup Nothing 0) base)

    it "moving a group's last child out prunes the now-empty group" do
      let m = run (Move "n5" Nothing 0) nested
      Map.lookup "n4" m.nodes `shouldEqual` Nothing
      (_.parent <$> Map.lookup "n5" m.nodes) `shouldEqual` Just Nothing
      m.roots `shouldEqual` [ "n5", "n1" ]

    it "deleting a group's last child prunes the now-empty group" do
      let m = run (Delete "n5") nested
      Map.lookup "n5" m.nodes `shouldEqual` Nothing
      Map.lookup "n4" m.nodes `shouldEqual` Nothing

    it "a renamed group emptied of children is kept" do
      let m = run (Move "n5" Nothing 0) (run (Rename "n4" "Keep") nested)
      (_.customTitle <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just "Keep")
      (_.children <$> Map.lookup "n4" m.nodes) `shouldEqual` Just []

    it "pruning cascades up, stopping at a renamed ancestor" do
      let
        deep = run (NewGroup (Just "n5") 0) nested -- group n6 inside n5 inside n4
        renamed = run (Rename "n4" "Keep") deep -- keep the outer group
        m = run (Move "n6" Nothing 0) renamed -- empty n5 -> prune n5 -> n4 empty but kept
      Map.lookup "n5" m.nodes `shouldEqual` Nothing
      (_.customTitle <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just "Keep")
      (_.children <$> Map.lookup "n4" m.nodes) `shouldEqual` Just []
