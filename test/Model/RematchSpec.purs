module Test.Model.RematchSpec where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Rematch (rematchOnStartup)
import Model.Tree (applyPatch)
import Model.Types (Model, RuntimeTab, RuntimeWindow, Kind(..), defaultNode, emptyModel, isLive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (\m e -> (applyBrowser 0.0 e m).model) emptyModel

-- last session: window n1 (id 1) with tabs n2 (A, tab 11) and n3 (B, tab 12)
prior :: Model
prior = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false ]

rt :: Int -> Int -> Int -> String -> RuntimeTab
rt tabId windowId index title =
  { tabId, windowId, index, url: Just ("http://" <> title), title, active: false, favIconUrl: Nothing, nodeKey: Nothing }

-- a runtime tab carrying a stamped node id (browser.sessions value), as it would
-- appear after a restart that session-restored the tab
rtKeyed :: Int -> Int -> Int -> String -> String -> RuntimeTab
rtKeyed tabId windowId index title nodeKey =
  { tabId, windowId, index, url: Just ("http://" <> title), title, active: false, favIconUrl: Nothing, nodeKey: Just nodeKey }

rw :: Int -> Array RuntimeTab -> RuntimeWindow
rw windowId tabs = { windowId, tabs }

rematch :: Array RuntimeWindow -> Model -> Model
rematch current m = (rematchOnStartup 1.0 current m).model

spec :: Spec Unit
spec = describe "Model.Rematch" do
  it "a clean restart re-binds existing nodes with fresh ids (no duplication)" do
    let m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "B" ] ] prior
    Map.size m.nodes `shouldEqual` 3
    m.roots `shouldEqual` [ "n1" ]
    (_.windowId <$> Map.lookup "n1" m.nodes) `shouldEqual` Just (Just 5)
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (_.tabId <$> Map.lookup "n3" m.nodes) `shouldEqual` Just (Just 52)
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    Map.lookup 51 m.byTab `shouldEqual` Just "n2"
    Map.lookup 5 m.byWindow `shouldEqual` Just "n1"

  it "matches a tab by its stamped node id even when the url changed" do
    -- A (n2) reopens under a DIFFERENT url but still carries its stamped node id "n2"
    -- (browser.sessions survived the restart). It binds to the same node, by id — no
    -- url guessing, no fresh duplicate — and stays in its window (matched by the key).
    let m = rematch [ rw 5 [ rtKeyed 51 5 0 "changed" "n2" ] ] prior
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    (_.parent <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "n1")
    -- no fresh node was created (n1 + n2 only; B/n3 genuinely didn't reopen -> dropped)
    Map.size m.nodes `shouldEqual` 2

  it "a stamped node id beats a colliding url (no mis-bind to a duplicate)" do
    -- two tabs share http://A; only one reopens, carrying the stamp for n3 (the
    -- SECOND A). It must bind to n3, not n2, despite both matching by url.
    let
      priorDup = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "A" false ]
      m = rematch [ rw 5 [ rtKeyed 51 5 0 "A" "n3" ] ] priorDup
    (_.tabId <$> Map.lookup "n3" m.nodes) `shouldEqual` Just (Just 51) -- the stamped one binds
    Map.lookup "n2" m.nodes `shouldEqual` Nothing -- the other A did not reopen -> dropped

  it "chooseWindow prefers the stamped node's window over a url tie" do
    -- two prior windows W1=[A], W2=[B]; a new window holds a tab stamped for A (W1)
    -- and a tab whose url is B (W2). The stamp must win: the window binds to W1 (n1).
    let
      prior2 = runEvents [ openTab 11 1 0 "A" true, openTab 21 2 0 "B" true ]
      m = rematch [ rw 5 [ rtKeyed 51 5 0 "whatever" "n2", rt 52 5 1 "B" ] ] prior2
    (_.windowId <$> Map.lookup "n1" m.nodes) `shouldEqual` Just (Just 5)
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)

  it "a stale nodeKey does not hijack a freshly-created node (prior-live only)" do
    -- empty prior (e.g. the data was reset); two new tabs, the second stamped with
    -- the id the first will be allocated. The stamp must NOT latch onto that fresh
    -- node — both tabs get distinct fresh nodes.
    let m = rematch [ rw 5 [ rt 51 5 0 "X", rtKeyed 52 5 1 "Y" "n2" ] ] emptyModel
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (_.tabId <$> Map.lookup "n3" m.nodes) `shouldEqual` Just (Just 52)

  it "a fresh tab orphaned in a reopened window is dropped (not kept)" do
    -- B (n3) was never restored, and its window reopened without it: drop it, closing
    -- the close-rule gap for a tab closed while the event page was suspended
    let m = rematch [ rw 5 [ rt 51 5 0 "A" ] ] prior
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    Map.lookup "n3" m.nodes `shouldEqual` Nothing
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2" ]

  it "a restored tab orphaned in a reopened window is kept (a fresh one would drop)" do
    -- mark A (n2) as restored-from-history; only B (n3) reopens in the window
    let
      priorR = prior { nodes = Map.update (\n -> Just (n { restoredFromClosed = true })) "n2" prior.nodes }
      m = rematch [ rw 5 [ rt 52 5 0 "B" ] ] priorR
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just true
    -- A (n2) did not reopen but is restored, so it is kept as closed history
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3" ]

  it "does not drop an orphan from a window whose url is shared (ambiguous match)" do
    -- two prior windows both hold X; only one reopens. The duplicate url can bind the
    -- wrong window node, so Y (fresh, in the other window) must be kept, never dropped.
    let
      prior2 = runEvents [ openTab 11 1 0 "X" true, openTab 21 2 0 "X" true, openTab 22 2 1 "Y" false ]
      m = rematch [ rw 5 [ rt 51 5 0 "X" ] ] prior2
    (_.title <$> Map.lookup "n5" m.nodes) `shouldEqual` Just "Y"
    (isLive <$> Map.lookup "n5" m.nodes) `shouldEqual` Just false

  it "does not drop an orphan when its window gained a fresh tab (url may have changed)" do
    -- A reopens; B reopens under a changed url, so it can't be url-matched and lands as
    -- a fresh node. B's old node may be that reopened tab, so it is kept, not dropped.
    let m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "B2" ] ] prior
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    (_.title <$> Map.lookup "n3" m.nodes) `shouldEqual` Just "B"
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false

  it "does not drop an orphan when a sibling moved in from another window (changed url)" do
    -- W1 [A,B], W2 [C]; W1 reopens as [A,C] where B's url changed to C. C is moved in
    -- from W2 (not a fresh tab), so the window is ambiguous and B must be kept, not dropped.
    let
      prior3 = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false, openTab 21 2 0 "C" true ]
      m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "C" ] ] prior3
    (_.title <$> Map.lookup "n3" m.nodes) `shouldEqual` Just "B"
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false

  it "does not drop an orphan when a fresh tab appeared in another window (moved + changed url)" do
    -- W1 [A,B], W2 [C]; current W1 [A], W2 [C, B2] (B moved to W2 and changed url to B2).
    -- B2 is a fresh tab elsewhere, so it might BE B reopened — keep B (n3), don't drop it.
    let
      prior4 = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false, openTab 21 2 0 "C" true ]
      m = rematch [ rw 5 [ rt 51 5 0 "A" ], rw 6 [ rt 61 6 0 "C", rt 62 6 1 "B2" ] ] prior4
    (_.title <$> Map.lookup "n3" m.nodes) `shouldEqual` Just "B"
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false

  it "a genuinely new tab gets a new node under its window" do
    let m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "B", rt 53 5 2 "C" ] ] prior
    Map.size m.nodes `shouldEqual` 4
    (_.kind <$> Map.lookup "n4" m.nodes) `shouldEqual` Just KTab
    (_.tabId <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just 53)
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3", "n4" ]

  it "a window that did not reopen closes as a restorable previous session" do
    let m = rematch [] prior
    (isLive <$> Map.lookup "n1" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false
    m.roots `shouldEqual` [ "n1" ]

  it "duplicate urls across windows don't steal each other's nodes" do
    let
      prior2 = runEvents
        [ openTab 11 1 0 "A" true, openTab 12 1 1 "A2" true, openTab 13 1 2 "S" true
        , openTab 21 2 0 "B" true, openTab 22 2 1 "B2" true, openTab 23 2 2 "S" true
        ]
      -- n4 = window 1's "S" tab, n8 = window 2's "S" tab (same url http://S)
      m = rematch
        [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "A2", rt 53 5 2 "S" ]
        , rw 6 [ rt 61 6 0 "B", rt 62 6 1 "B2", rt 63 6 2 "S" ]
        ]
        prior2
    Map.size m.nodes `shouldEqual` 8 -- no duplication
    (_.tabId <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just 53) -- window 1's S stays in window 1
    (_.tabId <$> Map.lookup "n8" m.nodes) `shouldEqual` Just (Just 63) -- window 2's S stays in window 2

  it "preserves user organization: a reopened window stays nested where the user put it" do
    let
      -- the user nested the window (g, id 1) inside a folder; its tab is n2. Nesting
      -- a window is a pure tree move; nesting a LIVE tab now drives the browser, so
      -- we build the organized tree directly here.
      organized = applyPatch
        { upserts:
            [ (defaultNode "folder" KGroup 0.0) { title = "Folder", children = [ "g" ] }
            , (defaultNode "g" KGroup 0.0) { windowId = Just 1, parent = Just "folder", title = "Window", children = [ "n2" ] }
            , (defaultNode "n2" KTab 0.0) { parent = Just "g", url = Just "http://A", title = "A", tabId = Just 11 }
            ]
        , removes: []
        , roots: Just [ "folder" ]
        }
        emptyModel
      m = rematch [ rw 5 [ rt 51 5 0 "A" ] ] organized
    (_.parent <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "g") -- still under its window
    (_.parent <$> Map.lookup "g" m.nodes) `shouldEqual` Just (Just "folder") -- folder nesting preserved
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (_.windowId <$> Map.lookup "g" m.nodes) `shouldEqual` Just (Just 5)

  it "consolidates one live window whose tabs came from different prior windows" do
    let
      -- last session: window n1 (id 1) had "alpha"; window n3 (id 2) had "gamma"
      prior2 = runEvents [ openTab 11 1 0 "alpha" true, openTab 21 2 0 "gamma" true ]
      -- this session: a single browser window (id 5) holds BOTH alpha and gamma
      m = rematch [ rw 5 [ rt 51 5 0 "alpha", rt 52 5 1 "gamma" ] ] prior2
    -- the chosen window node (n1) is the one live window, holding both tabs in order
    (_.windowId <$> Map.lookup "n1" m.nodes) `shouldEqual` Just (Just 5)
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n4" ]
    (_.parent <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "n1") -- alpha
    (_.parent <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just "n1") -- gamma moved in
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    (isLive <$> Map.lookup "n4" m.nodes) `shouldEqual` Just true
    -- the other prior window is closed and emptied (no live tab orphaned under it)
    (_.windowId <$> Map.lookup "n3" m.nodes) `shouldEqual` Just Nothing
    (_.children <$> Map.lookup "n3" m.nodes) `shouldEqual` Just []
