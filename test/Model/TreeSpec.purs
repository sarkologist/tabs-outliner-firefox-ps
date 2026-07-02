module Test.Model.TreeSpec where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Tree (applyPatch, insertAtClamped, insertAtLive, isLiveWindow, liveInsertIndex, moveWithin, rootAncestor, searchVisible, subtreeIds, visible)
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

node :: String -> Kind -> Array String -> Node
node id kind children = (defaultNode id kind 0.0) { children = children }

-- A -> [B, C]; C -> [D]
fixture :: Model
fixture = emptyModel
  { roots = [ "A" ]
  , nodes = Map.fromFoldable $ map (\n -> Tuple n.id n)
      [ node "A" KGroup [ "B", "C" ]
      , node "B" KTab []
      , node "C" KGroup [ "D" ]
      , node "D" KTab []
      ]
  }

collapse :: String -> Model -> Model
collapse id m = m { nodes = Map.update (\n -> Just (n { collapsed = true })) id m.nodes }

spec :: Spec Unit
spec = describe "Model.Tree" do
  describe "visible" do
    it "is preorder with depth when fully expanded" do
      visible fixture `shouldEqual`
        [ { id: "A", depth: 0 }
        , { id: "B", depth: 1 }
        , { id: "C", depth: 1 }
        , { id: "D", depth: 2 }
        ]
    it "does not descend collapsed nodes (so render is O(visible))" do
      visible (collapse "C" fixture) `shouldEqual`
        [ { id: "A", depth: 0 }
        , { id: "B", depth: 1 }
        , { id: "C", depth: 1 }
        ]

  describe "searchVisible" do
    it "shows matches and their ancestors, even inside collapsed groups" do
      let
        titled id title parent children =
          (defaultNode id KGroup 0.0) { title = title, parent = parent, children = children }
        m = emptyModel
          { roots = [ "A" ]
          , nodes = Map.fromFoldable $ map (\n -> Tuple n.id n)
              [ titled "A" "alpha" Nothing [ "B", "C" ]
              , titled "B" "beta" (Just "A") []
              , (titled "C" "gamma" (Just "A") [ "D" ]) { collapsed = true }
              , titled "D" "delta" (Just "C") []
              ]
          }
      map _.id (searchVisible "delta" m) `shouldEqual` [ "A", "C", "D" ]
      map _.id (searchVisible "bet" m) `shouldEqual` [ "A", "B" ]

  describe "subtreeIds" do
    it "collects the subtree including the root" do
      subtreeIds "C" fixture `shouldEqual` [ "C", "D" ]
      subtreeIds "A" fixture `shouldEqual` [ "A", "B", "C", "D" ]

  describe "rootAncestor" do
    -- a model whose nesting is wired via parent back-links (which rootAncestor
    -- walks): A -> [B, C]; C -> [D]
    let
      linked = applyPatch
        { upserts:
            [ (defaultNode "A" KGroup 0.0) { children = [ "B", "C" ] }
            , (defaultNode "B" KTab 0.0) { parent = Just "A" }
            , (defaultNode "C" KGroup 0.0) { parent = Just "A", children = [ "D" ] }
            , (defaultNode "D" KTab 0.0) { parent = Just "C" }
            ]
        , removes: []
        , roots: Just [ "A" ]
        }
        emptyModel
    it "walks up to the topmost ancestor" do
      rootAncestor "D" linked `shouldEqual` "A" -- D -> C -> A
      rootAncestor "B" linked `shouldEqual` "A" -- B -> A
    it "is the node itself when it has no parent" do
      rootAncestor "A" linked `shouldEqual` "A"
    it "stops on a parent cycle instead of looping forever (corruption backstop)" do
      let
        cyclic = applyPatch
          { upserts:
              [ (defaultNode "X" KGroup 0.0) { parent = Just "Y" }
              , (defaultNode "Y" KGroup 0.0) { parent = Just "X" }
              ]
          , removes: []
          , roots: Just []
          }
          emptyModel
      -- terminates at the first re-seen node rather than recursing forever
      rootAncestor "X" cyclic `shouldEqual` "X"

  describe "isLiveWindow" do
    -- a container is a window exactly while it directly owns a live tab; the
    -- owning window of a node is its immediate parent (nesting is allowed, with
    -- no special "nearest window ancestor" walk)
    it "is true for a container with a live tab child, false otherwise" do
      let
        liveTab = (defaultNode "t" KTab 0.0) { tabId = Just 1 }
        win = (defaultNode "w" KGroup 0.0) { children = [ "t" ] }
        empty = (defaultNode "g" KGroup 0.0)
        closedChild = (defaultNode "c" KTab 0.0)
        grp = (defaultNode "h" KGroup 0.0) { children = [ "c" ] }
        m = applyPatch
          { upserts: [ liveTab, win, empty, closedChild, grp ]
          , removes: []
          , roots: Just [ "w", "g", "h" ]
          }
          emptyModel
      (isLiveWindow m <$> Map.lookup "w" m.nodes) `shouldEqual` Just true
      (isLiveWindow m <$> Map.lookup "g" m.nodes) `shouldEqual` Just false -- empty container
      (isLiveWindow m <$> Map.lookup "h" m.nodes) `shouldEqual` Just false -- only a closed-tab child
      (isLiveWindow m <$> Map.lookup "t" m.nodes) `shouldEqual` Just false -- a tab is never a window

  describe "array helpers" do
    -- the live-index mapping that keeps `filter live children` in browser order
    -- when closed nodes (here encoded as 0) are interleaved among live ones (> 0)
    let
      live :: Int -> Boolean
      live i = i > 0
    it "insertAtClamped clamps out-of-range indices" do
      insertAtClamped 99 "x" [ "a", "b" ] `shouldEqual` [ "a", "b", "x" ]
      insertAtClamped (-5) "x" [ "a", "b" ] `shouldEqual` [ "x", "a", "b" ]
    it "moveWithin relocates an element (all live: live-index == array-index)" do
      moveWithin (const true) "a" 2 [ "a", "b", "c" ] `shouldEqual` [ "b", "c", "a" ]
      moveWithin (const true) "c" 0 [ "a", "b", "c" ] `shouldEqual` [ "c", "a", "b" ]
    it "liveInsertIndex maps a live index past interleaved closed nodes" do
      -- [0, 1, 0, 2]: live elements 1, 2 sit at array indices 1 and 3
      liveInsertIndex live 0 [ 0, 1, 0, 2 ] `shouldEqual` 1 -- before the 1st live
      liveInsertIndex live 1 [ 0, 1, 0, 2 ] `shouldEqual` 3 -- before the 2nd live
      liveInsertIndex live 2 [ 0, 1, 0, 2 ] `shouldEqual` 4 -- past the last live -> append
      liveInsertIndex live 0 [ 0, 0 ] `shouldEqual` 2 -- no live element: append after the closed ones
      liveInsertIndex live 5 [ 1, 2 ] `shouldEqual` 2 -- index past the end clamps to append
      liveInsertIndex live (-1) [ 0, 1 ] `shouldEqual` 1 -- negative clamps to before the 1st live
    it "insertAtLive lands a value at the given live index, closed nodes fixed" do
      insertAtLive live 0 9 [ 0, 1, 0, 2 ] `shouldEqual` [ 0, 9, 1, 0, 2 ]
      insertAtLive live 1 9 [ 0, 1, 0, 2 ] `shouldEqual` [ 0, 1, 0, 9, 2 ]
      insertAtLive live 2 9 [ 0, 1, 0, 2 ] `shouldEqual` [ 0, 1, 0, 2, 9 ]
    it "moveWithin keeps the live subsequence ordered across interleaved closed nodes" do
      -- live 1,2,3 interleaved with a closed 0; move live 1 to live-index 2
      moveWithin live 1 2 [ 1, 0, 2, 3 ] `shouldEqual` [ 0, 2, 3, 1 ]
      -- move live 3 to the front of the live order (live-index 0)
      moveWithin live 3 0 [ 1, 0, 2, 3 ] `shouldEqual` [ 3, 1, 0, 2 ]

  describe "applyPatch" do
    it "upserts, removes, and updates roots + indexes" do
      let
        liveTab = (defaultNode "T" KTab 0.0) { tabId = Just 7 }
        m = applyPatch { upserts: [ liveTab ], removes: [], roots: Just [ "T" ] } emptyModel
      m.roots `shouldEqual` [ "T" ]
      Map.lookup "T" m.nodes `shouldEqual` Just liveTab
      Map.lookup 7 m.byTab `shouldEqual` Just "T"
      let m2 = applyPatch { upserts: [], removes: [ "T" ], roots: Just [] } m
      Map.lookup "T" m2.nodes `shouldEqual` Nothing
      m2.roots `shouldEqual` []
