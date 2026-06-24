module Test.Model.DropSpec where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Command (Command(..))
import Model.Drop (dropCommand, dropPlacement)
import Model.Tree (visible)
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

node :: String -> Kind -> Maybe String -> Array String -> Node
node id kind parent children = (defaultNode id kind 0.0) { parent = parent, children = children }

-- Command has no Eq/Show; project a Move to a comparable record (Nothing otherwise).
moveOf :: Command -> Maybe { nid :: String, parent :: Maybe String, index :: Int }
moveOf = case _ of
  Move nid parent index -> Just { nid, parent, index }
  _ -> Nothing

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

-- W(window) -> [A(tab), G(group) -> [C(tab)]]
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

collapse :: String -> Model -> Model
collapse id m = m { nodes = Map.update (\n -> Just (n { collapsed = true })) id m.nodes }

spec :: Spec Unit
spec = describe "Model.Drop" do
  describe "dropPlacement" do
    -- visible: [W@0, A@1, G@2, C@3]
    it "drops onto a group as its last child (line below the subtree, one deeper)" do
      dropPlacement fixture (visible fixture) "A" "G"
        `shouldEqual` Just { atIndex: 4, depth: 2 }

    it "drops onto a non-group as a sibling before it (line at the target's depth)" do
      dropPlacement fixture (visible fixture) "C" "A"
        `shouldEqual` Just { atIndex: 1, depth: 1 }

    it "appends right under a collapsed group (its subtree isn't visible)" do
      let m = collapse "G" fixture
      dropPlacement m (visible m) "A" "G"
        `shouldEqual` Just { atIndex: 3, depth: 2 }

    it "has no placement when dropping onto itself" do
      dropPlacement fixture (visible fixture) "A" "A" `shouldEqual` Nothing

    it "has no placement when dropping into its own subtree (would be a cycle)" do
      dropPlacement fixture (visible fixture) "W" "C" `shouldEqual` Nothing

  describe "dropCommand" do
    -- the index must land the node where the preview says: immediately BEFORE the
    -- target. `move` deletes the node first, so dragging downward past a sibling
    -- needs the target's index in the post-removal list, not the original.
    it "drops downward past a sibling, landing before the target (not after)" do
      -- drag A onto C in [A,B,C]: A removed -> [B,C], C at 1 -> insert -> [B,A,C]
      moveOf (dropCommand "A" (node "C" KTab (Just "W") []) siblings3)
        `shouldEqual` Just { nid: "A", parent: Just "W", index: 1 }

    it "drops upward before the target" do
      -- drag C onto A: C removed -> [A,B], A at 0 -> insert -> [C,A,B]
      moveOf (dropCommand "C" (node "A" KTab (Just "W") []) siblings3)
        `shouldEqual` Just { nid: "C", parent: Just "W", index: 0 }

    it "drops onto a group as its last child" do
      moveOf (dropCommand "A" (node "G" KGroup (Just "W") [ "C" ]) fixture)
        `shouldEqual` Just { nid: "A", parent: Just "G", index: 1 }

    it "drops onto a window (a container) as its last child, not a sibling before it" do
      -- a window is just a container now, so a drop onto W lands inside it; the old
      -- KWindow path inserted before W as a root sibling (parent Nothing)
      moveOf (dropCommand "B" (node "W" KGroup Nothing [ "A", "B", "C" ]) siblings3)
        `shouldEqual` Just { nid: "B", parent: Just "W", index: 3 }
