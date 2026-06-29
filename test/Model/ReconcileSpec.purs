module Test.Model.ReconcileSpec where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Tree (applyPatch, insertAtClamped)
import Model.Types (Kind(..), Model, Node, NodeId, defaultNode, emptyModel, isLive, isLiveTab)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

step :: BrowserEvent -> Model -> Model
step e m = (applyBrowser 0.0 e m).model

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (flip step) emptyModel

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

-- Fixture builders for models with closed/history nodes interleaved among a
-- window's live tabs. `nextId` is set past the listed ids so the first
-- event-created node gets a fresh id (events allocate "n" <> show nextId).
modelOf :: Array Node -> Array NodeId -> Int -> Model
modelOf nodes roots nextId =
  (applyPatch { upserts: nodes, removes: [], roots: Just roots } emptyModel) { nextId = nextId }

win :: NodeId -> Int -> Array NodeId -> Node
win id windowId children = (defaultNode id KGroup 0.0) { windowId = Just windowId, title = "Window", children = children }

liveTab :: NodeId -> NodeId -> Int -> String -> Node
liveTab id parent tabId title =
  (defaultNode id KTab 0.0) { parent = Just parent, tabId = Just tabId, title = title, url = Just ("http://" <> title) }

closedTab :: NodeId -> NodeId -> String -> Node
closedTab id parent title =
  (defaultNode id KTab 0.0) { parent = Just parent, title = title, url = Just ("http://" <> title), closedAt = Just 0.0 }

-- the children array of a window node, unchanged (live tabs + interleaved closed nodes)
childrenOf :: Model -> NodeId -> Array NodeId
childrenOf m wid = fromMaybe [] (_.children <$> Map.lookup wid m.nodes)

-- the live-tab subsequence of a window's children, by title (must equal browser order)
liveTitles :: Model -> NodeId -> Array String
liveTitles m wid = Array.mapMaybe titleIfLive (childrenOf m wid)
  where
  titleIfLive cid = Map.lookup cid m.nodes >>= \n -> if isLiveTab n then Just n.title else Nothing

-- Property-test harness: drive one window (id 1) through a stream of open/move
-- events while keeping a plain browser-order oracle (`order` — the live tab ids in
-- index order). The model starts with closed nodes interleaved among the live
-- tabs; the live subsequence of its children must stay equal to `order`.
type SimState = { model :: Model, order :: Array Int, nextTab :: Int }

-- window 1 holds a live tab (browser id 100) flanked by two closed history nodes
simInit :: SimState
simInit =
  { model: modelOf
      [ win "n1" 1 [ "nc1", "n2", "nc2" ]
      , closedTab "nc1" "n1" "closed1"
      , liveTab "n2" "n1" 100 "t100"
      , closedTab "nc2" "n1" "closed2"
      ]
      [ "n1" ]
      3
  , order: [ 100 ]
  , nextTab: 101
  }

-- non-negative modulo (Arbitrary Int can be negative; keep indices in range)
pmod :: Int -> Int -> Int
pmod a b = if b <= 0 then 0 else ((a `mod` b) + b) `mod` b

-- Interpret one op. Odd first parameter (when there is a live tab to move) => a
-- move; otherwise an open. Indices are taken modulo the live count so every op is
-- valid. The oracle `order` is updated with the same browser semantics.
simStep :: SimState -> Array Int -> SimState
simStep s a =
  let
    len = Array.length s.order
    g i = fromMaybe 0 (Array.index a i)
  in
    if len > 0 && pmod (g 0) 2 == 1 then
      let
        from = fromMaybe 0 (Array.index s.order (pmod (g 1) len))
        toIndex = pmod (g 2) len
        model' = (applyBrowser 0.0 (TabMoved { tabId: from, windowId: 1, toIndex }) s.model).model
      in
        s { model = model', order = insertAtClamped toIndex from (Array.delete from s.order) }
    else
      let
        tabId = s.nextTab
        idx = pmod (g 1) (len + 1)
        ev = TabOpened
          { tabId, windowId: 1, index: idx, url: Just ("http://t" <> show tabId), title: "t" <> show tabId, active: false, favIconUrl: Nothing }
        model' = (applyBrowser 0.0 ev s.model).model
      in
        { model: model', order: insertAtClamped idx tabId s.order, nextTab: s.nextTab + 1 }

-- the live tabs of window 1, in children-array order, as their browser tab ids
liveOrder :: Model -> Array Int
liveOrder m = Array.mapMaybe (\cid -> Map.lookup cid m.nodes >>= \n -> if isLiveTab n then n.tabId else Nothing)
  (childrenOf m "n1")

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

  it "drops a freshly-opened tab closed by the browser (never restored)" do
    -- the close rule: a browser-closed tab is kept only if it was restored from
    -- history; a fresh tab the user just opens and closes is discarded, not saved
    let m = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false, TabClosed { tabId: 11 } ]
    -- A (n2) is removed entirely, not kept as crossed-out history
    Map.lookup "n2" m.nodes `shouldEqual` Nothing
    -- its sibling B (n3) and the window remain
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just true
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n3" ]

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
    -- a closed tab n2 under a live window n1, plus a pending-restore slot for it that
    -- did NOT come from Command.restore (e.g. a live-tab rehome). The rebind itself
    -- must NOT set restoredFromClosed; unflagged, a later browser close then drops it.
    let
      m0 = applyPatch
        { upserts:
            [ (defaultNode "n1" KGroup 0.0) { windowId = Just 1, title = "Window", children = [ "n2" ] }
            , (defaultNode "n2" KTab 0.0) { parent = Just "n1", url = Just "http://A", title = "A", closedAt = Just 0.0 }
            ]
        , removes: []
        , roots: Just [ "n1" ]
        }
        emptyModel
      m1 = m0 { pendingRestore = Map.singleton 1 (List.singleton "n2") }
      reopened = (applyBrowser 0.0 (openTab 99 1 0 "A" true) m1).model
    -- n2 rebound to the new browser tab, but NOT flagged restored
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 99)
    (_.restoredFromClosed <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just false
    -- unflagged, so the browser close drops it (it was never a user restore)
    let closedAgain = (applyBrowser 0.0 (TabClosed { tabId: 99 }) reopened).model
    Map.lookup "n2" closedAgain.nodes `shouldEqual` Nothing

  it "a reused browser tab id creates a fresh node (no stale-index no-op)" do
    let
      m = runEvents
        [ openTab 11 1 0 "A" true -- n2 bound to tab 11
        , TabClosed { tabId: 11 } -- fresh tab dropped; byTab[11] now stale
        , openTab 11 1 0 "C" true -- browser reuses id 11 for a brand-new tab
        ]
    -- the dropped node is gone, not resurrected by the reused id
    Map.lookup "n2" m.nodes `shouldEqual` Nothing
    (isLive <$> Map.lookup "n4" m.nodes) `shouldEqual` Just true
    Map.lookup 11 m.byTab `shouldEqual` Just "n4"

  describe "live tab order is kept across interleaved closed nodes" do
    -- The invariant: for a window node, `filter isLiveTab children` (in array
    -- order) equals the window's browser tabs in `index` order. A browser tab
    -- index counts LIVE tabs only, so it must be mapped past any interleaved
    -- closed/history nodes when a tab opens / moves / attaches.

    it "opens a fresh tab at its browser index, past an interleaved closed node" do
      let
        -- browser order is A(0), B(1); a closed node C sits between them
        m0 = modelOf
          [ win "n1" 1 [ "n2", "nc", "n3" ]
          , liveTab "n2" "n1" 11 "A"
          , closedTab "nc" "n1" "C"
          , liveTab "n3" "n1" 12 "B"
          ]
          [ "n1" ]
          4
        -- open D at browser index 1 (between A and B)
        m = (applyBrowser 0.0 (openTab 13 1 1 "D" false) m0).model
      liveTitles m "n1" `shouldEqual` [ "A", "D", "B" ] -- D became the 2nd live tab
      childrenOf m "n1" `shouldEqual` [ "n2", "nc", "n4", "n3" ] -- closed C unmoved

    it "appends a fresh tab after the last live tab and trailing closed nodes" do
      let
        m0 = modelOf
          [ win "n1" 1 [ "n2", "nc" ]
          , liveTab "n2" "n1" 11 "A"
          , closedTab "nc" "n1" "C"
          ]
          [ "n1" ]
          4
        -- open B at browser index 1 (the end of the single live tab)
        m = (applyBrowser 0.0 (openTab 12 1 1 "B" false) m0).model
      liveTitles m "n1" `shouldEqual` [ "A", "B" ]
      childrenOf m "n1" `shouldEqual` [ "n2", "nc", "n4" ]

    it "moves a live tab to a browser index, keeping live order across a closed node" do
      let
        m0 = modelOf
          [ win "n1" 1 [ "n2", "n3", "nc", "n4" ]
          , liveTab "n2" "n1" 11 "A"
          , liveTab "n3" "n1" 12 "B"
          , closedTab "nc" "n1" "X"
          , liveTab "n4" "n1" 13 "C"
          ]
          [ "n1" ]
          5
        -- browser order A(0), B(1), C(2); move A to index 2 -> B, C, A
        m = (applyBrowser 0.0 (TabMoved { tabId: 11, windowId: 1, toIndex: 2 }) m0).model
      liveTitles m "n1" `shouldEqual` [ "B", "C", "A" ]
      childrenOf m "n1" `shouldEqual` [ "n3", "nc", "n4", "n2" ] -- closed X stays put

    it "attaches a tab at its browser index in the destination window, past a closed node" do
      let
        m0 = modelOf
          [ win "n1" 1 [ "n2" ]
          , liveTab "n2" "n1" 11 "A"
          , win "n3" 2 [ "n4", "nc", "n5" ]
          , liveTab "n4" "n3" 21 "B"
          , closedTab "nc" "n3" "X"
          , liveTab "n5" "n3" 22 "C"
          ]
          [ "n1", "n3" ]
          6
        -- move A into window 2 at browser index 1 (between B and C)
        m = (applyBrowser 0.0 (TabAttached { tabId: 11, windowId: 2, index: 1 }) m0).model
      liveTitles m "n3" `shouldEqual` [ "B", "A", "C" ]
      childrenOf m "n3" `shouldEqual` [ "n4", "nc", "n2", "n5" ]
      Map.lookup "n1" m.nodes `shouldEqual` Nothing -- window 1 emptied and was pruned

    it "property: the live subsequence equals browser order after random opens/moves" $
      -- Each generated op is an `Array Int` (parameters, padded with 0). A model
      -- that starts with closed nodes interleaved is driven by the same op stream
      -- as a plain browser-order oracle; the live subsequence must track it.
      quickCheck \(ops :: Array (Array Int)) ->
        let s = foldl simStep simInit ops
        in liveOrder s.model === s.order

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
