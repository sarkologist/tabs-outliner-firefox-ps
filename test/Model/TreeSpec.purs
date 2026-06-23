module Test.Model.TreeSpec where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Tree (applyPatch, insertAtClamped, moveWithin, subtreeIds, visible)
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

  describe "subtreeIds" do
    it "collects the subtree including the root" do
      subtreeIds "C" fixture `shouldEqual` [ "C", "D" ]
      subtreeIds "A" fixture `shouldEqual` [ "A", "B", "C", "D" ]

  describe "array helpers" do
    it "insertAtClamped clamps out-of-range indices" do
      insertAtClamped 99 "x" [ "a", "b" ] `shouldEqual` [ "a", "b", "x" ]
      insertAtClamped (-5) "x" [ "a", "b" ] `shouldEqual` [ "x", "a", "b" ]
    it "moveWithin relocates an element" do
      moveWithin "a" 2 [ "a", "b", "c" ] `shouldEqual` [ "b", "c", "a" ]
      moveWithin "c" 0 [ "a", "b", "c" ] `shouldEqual` [ "c", "a", "b" ]

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
