module Test.Model.ReconcileSpec where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Tree (applyPatch)
import Model.Types (Model, emptyModel, isLive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

step :: BrowserEvent -> Model -> Model
step e m = (applyBrowser 0.0 e m).model

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (flip step) emptyModel

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

spec :: Spec Unit
spec = describe "Model.Reconcile" do
  it "lazily creates a window node when a tab opens" do
    let m = runEvents [ openTab 11 1 0 "A" true ]
    m.roots `shouldEqual` [ "n1" ]
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2" ]
    (_.parent <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "n1")
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    Map.lookup 11 m.byTab `shouldEqual` Just "n2"
    Map.lookup 1 m.byWindow `shouldEqual` Just "n1"

  it "does not duplicate a window opened explicitly then populated" do
    let m = runEvents [ WindowOpened { windowId: 1 }, openTab 11 1 0 "A" true ]
    Array.length m.roots `shouldEqual` 1
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2" ]

  it "keeps a closed tab in place as history" do
    let m = runEvents [ openTab 11 1 0 "A" true, TabClosed { tabId: 11 } ]
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just Nothing
    -- still a child of its window (not detached)
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2" ]

  it "updates title/url on change" do
    let m = runEvents [ openTab 11 1 0 "A" true, TabChanged { tabId: 11, title: Just "A2", url: Just "http://a2", favIconUrl: Nothing } ]
    (_.title <$> Map.lookup "n2" m.nodes) `shouldEqual` Just "A2"
    (_.url <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "http://a2")

  it "moves the active flag on activation" do
    let
      m = runEvents
        [ openTab 11 1 0 "A" true
        , openTab 12 1 1 "B" false
        , TabActivated { tabId: 12, windowId: 1 }
        ]
    (_.active <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (_.active <$> Map.lookup "n3" m.nodes) `shouldEqual` Just true

  it "reorders within a window on move" do
    let
      m = runEvents
        [ openTab 11 1 0 "A" true
        , openTab 12 1 1 "B" false
        , TabMoved { tabId: 12, windowId: 1, toIndex: 0 }
        ]
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n3", "n2" ]

  it "re-parents a tab across windows on attach, pruning the emptied window" do
    let
      m = runEvents
        [ openTab 11 1 0 "A" true
        , openTab 21 2 0 "B" true
        , TabAttached { tabId: 11, windowId: 2, index: 1 }
        ]
      win2 = Map.lookup "n3" m.nodes
    -- window 1 emptied when its only tab left, so it is pruned (an empty window
    -- can't exist in the browser anyway)
    Map.lookup "n1" m.nodes `shouldEqual` Nothing
    m.roots `shouldEqual` [ "n3" ]
    (_.children <$> win2) `shouldEqual` Just [ "n4", "n2" ] -- the tab joined window 2
    (_.parent <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "n3")

  it "attaching a tab to a never-announced window lazily creates that window node" do
    -- the tab tear-off path: a tab is dragged out to a brand-new window that was
    -- never seen via WindowOpened, and only the attach (resolved from the detach)
    -- arrives. resolveWindow must mint the window node so the move is tracked.
    let
      m = runEvents
        [ openTab 11 1 0 "A" true
        , openTab 12 1 1 "B" false
        , TabAttached { tabId: 12, windowId: 2, index: 0 }
        ]
    m.roots `shouldEqual` [ "n1", "n4" ]
    (_.windowId <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just 2)
    (_.children <$> Map.lookup "n4" m.nodes) `shouldEqual` Just [ "n3" ]
    (_.parent <$> Map.lookup "n3" m.nodes) `shouldEqual` Just (Just "n4")
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2" ] -- window 1 keeps A
    Map.lookup 2 m.byWindow `shouldEqual` Just "n4"

  it "closes a window subtree to history but keeps it as a root" do
    let
      m = runEvents
        [ openTab 11 1 0 "A" true
        , openTab 12 1 1 "B" false
        , WindowClosed { windowId: 1 }
        ]
    m.roots `shouldEqual` [ "n1" ]
    (isLive <$> Map.lookup "n1" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false

  it "a pending-queue rebind is not itself marked restored (only Command.restore marks)" do
    -- a closed tab n2 with a pending-restore slot for its window that did NOT come
    -- from a user restore (Command.restore is what flags the tabs it reopens). The
    -- rebind itself must NOT set restoredFromClosed, so that a stray queued node can
    -- never be turned into one a later browser close would wrongly DROP.
    let
      m0 = runEvents [ openTab 11 1 0 "A" true, TabClosed { tabId: 11 } ]
      m1 = m0 { pendingRestore = Map.singleton 1 (List.singleton "n2") }
      reopened = (applyBrowser 0.0 (openTab 99 1 0 "A" true) m1).model
    -- n2 rebound to the new browser tab...
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 99)
    -- ...but is NOT flagged restored, so a browser close keeps it as history
    (_.restoredFromClosed <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just false
    let closedAgain = (applyBrowser 0.0 (TabClosed { tabId: 99 }) reopened).model
    (isLive <$> Map.lookup "n2" closedAgain.nodes) `shouldEqual` Just false
    (_.children <$> Map.lookup "n1" closedAgain.nodes) `shouldEqual` Just [ "n2" ]

  it "a reused browser tab id creates a fresh node (no stale-index no-op)" do
    let
      m = runEvents
        [ openTab 11 1 0 "A" true -- n2 bound to tab 11
        , TabClosed { tabId: 11 } -- byTab[11] now points at a closed node (stale)
        , openTab 11 1 0 "C" true -- browser reuses id 11 for a brand-new tab
        ]
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just true
    Map.lookup 11 m.byTab `shouldEqual` Just "n3"

  describe "patch is O(change)" do
    it "a tab change touches exactly one node" do
      let
        m = runEvents [ openTab 11 1 0 "A" true ]
        p = (applyBrowser 0.0 (TabChanged { tabId: 11, title: Just "X", url: Nothing, favIconUrl: Nothing }) m).patch
      Array.length p.upserts `shouldEqual` 1
      Array.length p.removes `shouldEqual` 0

  describe "background/sidebar consistency by construction" do
    it "folding patches onto a view yields the same nodes & roots as the authority" do
      let
        events =
          [ openTab 11 1 0 "A" true
          , openTab 12 1 1 "B" false
          , openTab 21 2 0 "C" true
          , TabActivated { tabId: 12, windowId: 1 }
          , TabMoved { tabId: 12, windowId: 1, toIndex: 0 }
          , TabChanged { tabId: 21, title: Just "C2", url: Nothing, favIconUrl: Nothing }
          , TabClosed { tabId: 11 }
          , TabAttached { tabId: 21, windowId: 1, index: 0 }
          , WindowClosed { windowId: 2 }
          ]
        go acc e =
          let s = applyBrowser 0.0 e acc.auth
          in { auth: s.model, view: applyPatch s.patch acc.view }
        result = foldl go { auth: emptyModel, view: emptyModel } events
      result.view.nodes `shouldEqual` result.auth.nodes
      result.view.roots `shouldEqual` result.auth.roots
