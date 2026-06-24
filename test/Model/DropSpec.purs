module Test.Model.DropSpec where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Drop (dropPlacement)
import Model.Tree (visible)
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

node :: String -> Kind -> Maybe String -> Array String -> Node
node id kind parent children = (defaultNode id kind 0.0) { parent = parent, children = children }

-- W(window) -> [A(tab), G(group) -> [C(tab)]]
fixture :: Model
fixture = emptyModel
  { roots = [ "W" ]
  , nodes = Map.fromFoldable $ map (\n -> Tuple n.id n)
      [ node "W" KWindow Nothing [ "A", "G" ]
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
