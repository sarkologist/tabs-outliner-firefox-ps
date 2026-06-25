module Test.Model.DropSpec where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Command (Command(..), applyCommand)
import Model.Drop (dropPlacement)
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Model.View (Row)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

node :: String -> Kind -> Maybe String -> Array String -> Node
node id kind parent children = (defaultNode id kind 0.0) { parent = parent, children = children }

-- a projected row, only the fields dropPlacement reads
row :: String -> Int -> Int -> Int -> Kind -> Row
row id index depth subtreeEnd kind =
  { id, index, depth, subtreeEnd, kind, title: "", live: false, active: false, collapsed: false, hasChildren: false, isLastRoot: false }

childrenOf :: String -> Model -> Maybe (Array String)
childrenOf id m = _.children <$> Map.lookup id m.nodes

-- W(window) -> [A, B, C], three tab siblings
siblings3 :: Model
siblings3 = emptyModel
  { roots = [ "W" ]
  , nodes = Map.fromFoldable $ map (\n -> Tuple n.id n)
      [ node "W" KGroup Nothing [ "A", "B", "C" ]
      , node "A" KTab (Just "W") []
      , node "B" KTab (Just "W") []
      , node "C" KTab (Just "W") []
      ]
  }

-- W(window) -> [A(tab), G(group) -> [C(tab)]]; visible order [W@0,A@1,G@2,C@3]
fixture :: Model
fixture = emptyModel
  { roots = [ "W" ]
  , nodes = Map.fromFoldable $ map (\n -> Tuple n.id n)
      [ node "W" KGroup Nothing [ "A", "G" ]
      , node "A" KTab (Just "W") []
      , node "G" KGroup (Just "W") [ "C" ]
      , node "C" KTab (Just "G") []
      ]
  }

spec :: Spec Unit
spec = describe "Model.Drop" do
  describe "dropPlacement" do
    it "drops onto a group as its last child (line below the subtree, one deeper)" do
      -- drag A (span [1,2)) onto group G (idx 2, depth 1, subtreeEnd 4)
      dropPlacement { index: 1, subtreeEnd: 2 } (row "G" 2 1 4 KGroup)
        `shouldEqual` Just { atIndex: 4, depth: 2 }

    it "drops onto a non-group as a sibling before it (line at the target's depth)" do
      dropPlacement { index: 3, subtreeEnd: 4 } (row "A" 1 1 2 KTab)
        `shouldEqual` Just { atIndex: 1, depth: 1 }

    it "appends right under a collapsed group (its subtree isn't visible)" do
      -- collapsed G has no visible children, so its subtreeEnd is 3
      dropPlacement { index: 1, subtreeEnd: 2 } (row "G" 2 1 3 KGroup)
        `shouldEqual` Just { atIndex: 3, depth: 2 }

    it "has no placement when dropping onto itself" do
      dropPlacement { index: 1, subtreeEnd: 2 } (row "A" 1 1 2 KTab) `shouldEqual` Nothing

    it "has no placement when dropping into its own subtree (would be a cycle)" do
      -- drag W (span [0,4)) onto C (idx 3, inside the span)
      dropPlacement { index: 0, subtreeEnd: 4 } (row "C" 3 2 4 KTab) `shouldEqual` Nothing

  describe "Drop command (resolved by applyCommand)" do
    it "drops downward past a sibling, landing before the target (not after)" do
      -- drag A onto C in [A,B,C]: A removed -> [B,C], insert before C -> [B,A,C]
      childrenOf "W" (applyCommand 0.0 (Drop "A" "C") siblings3).model
        `shouldEqual` Just [ "B", "A", "C" ]

    it "drops upward before the target" do
      childrenOf "W" (applyCommand 0.0 (Drop "C" "A") siblings3).model
        `shouldEqual` Just [ "C", "A", "B" ]

    it "drops onto a group as its last child" do
      let m = (applyCommand 0.0 (Drop "A" "G") fixture).model
      childrenOf "G" m `shouldEqual` Just [ "C", "A" ]
      childrenOf "W" m `shouldEqual` Just [ "G" ]

    it "drops onto a container window as its last child, not a sibling before it" do
      childrenOf "W" (applyCommand 0.0 (Drop "B" "W") siblings3).model
        `shouldEqual` Just [ "A", "C", "B" ]

    it "a self-drop is a no-op (doesn't shuffle the node to the end)" do
      childrenOf "W" (applyCommand 0.0 (Drop "A" "A") siblings3).model
        `shouldEqual` Just [ "A", "B", "C" ]
