-- | The asymptotics guard. Builds a large forest (52k nodes) and asserts that a
-- | single browser event produces a bounded patch (records written/broadcast),
-- | and that `visible` over a collapsed forest is O(visible), not O(total).
-- | This is how the no-O(total) directive is enforced without manual testing.
module Test.Model.GuardSpec where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Tree (visible)
import Model.Types (Kind(..), Model, defaultNode, emptyModel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- w windows, each with k tabs. Browser ids: window i -> i, tab (i,j) -> i*1000+j.
bigForest :: Int -> Int -> Model
bigForest w k =
  let
    win i =
      let
        wid = "w" <> show i
        tabIds = map (\j -> "t" <> show i <> "_" <> show j) (Array.range 1 k)
        winNode = (defaultNode wid KGroup 0.0) { windowId = Just i, children = tabIds, title = "W" <> show i }
        tabs = map
          ( \j ->
              let
                tid = "t" <> show i <> "_" <> show j
              in
                (defaultNode tid KTab 0.0)
                  { tabId = Just (i * 1000 + j), parent = Just wid, title = tid, url = Just ("http://x/" <> tid) }
          )
          (Array.range 1 k)
      in
        { winNode, tabs }
    built = map win (Array.range 1 w)
    allNodes = Array.concatMap (\b -> Array.cons b.winNode b.tabs) built
  in
    emptyModel
      { roots = map (\i -> "w" <> show i) (Array.range 1 w)
      , nodes = Map.fromFoldable (map (\n -> Tuple n.id n) allNodes)
      , byTab = Map.fromFoldable (Array.concatMap (\b -> map (\t -> Tuple (fromMaybe 0 t.tabId) t.id) b.tabs) built)
      , byWindow = Map.fromFoldable (map (\b -> Tuple (fromMaybe 0 b.winNode.windowId) b.winNode.id) built)
      , nextId = w * k + w + 1
      }

collapseRoots :: Model -> Model
collapseRoots m = m
  { nodes = foldl (\nm rid -> Map.update (\n -> Just (n { collapsed = true })) rid nm) m.nodes m.roots }

spec :: Spec Unit
spec = describe "asymptotics guard (52k nodes)" do
  let
    w = 2000
    k = 25
    big = bigForest w k
    -- a tab in the middle of the forest
    midTab = 1000 * 1000 + 13
    midWin = 1000

  it "the fixture really is large" do
    Map.size big.nodes `shouldEqual` (w * k + w)

  it "a tab change touches exactly one node, regardless of forest size" do
    let p = (applyBrowser 0.0 (TabChanged { tabId: midTab, title: Just "X", url: Nothing, favIconUrl: Nothing }) big).patch
    Array.length p.upserts `shouldEqual` 1
    Array.length p.removes `shouldEqual` 0

  it "a tab close touches exactly one node" do
    let p = (applyBrowser 0.0 (TabClosed { tabId: midTab }) big).patch
    Array.length p.upserts `shouldEqual` 1

  it "opening a tab touches only its window + the new node (2)" do
    let
      ev = TabOpened { tabId: 7777777, windowId: midWin, index: 0, url: Just "http://new", title: "new", active: false, favIconUrl: Nothing }
      p = (applyBrowser 0.0 ev big).patch
    Array.length p.upserts `shouldEqual` 2

  it "visible over a fully-collapsed forest is O(visible), not O(total)" do
    -- only the window rows are visible; none of the 50k tabs are walked
    Array.length (visible (collapseRoots big)) `shouldEqual` w
