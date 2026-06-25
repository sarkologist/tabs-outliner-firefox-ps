module Test.Model.ViewSpec where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Model.View (computeOrder, focusIndexOf, sliceView)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

node :: String -> Kind -> Maybe String -> Array String -> Node
node id kind parent children = (defaultNode id kind 0.0) { parent = parent, children = children }

-- roots [W, G]; W (live window 1) -> [A(active tab), B], G (group) -> [C]
-- visible order: [W@0, A@1, B@2, G@3, C@4]
m :: Model
m = emptyModel
  { roots = [ "W", "G" ]
  , nodes = Map.fromFoldable $ map (\n -> Tuple n.id n)
      [ (node "W" KGroup Nothing [ "A", "B" ]) { windowId = Just 1, title = "W" }
      , (node "A" KTab (Just "W") []) { tabId = Just 11, active = true, title = "A" }
      , (node "B" KTab (Just "W") []) { title = "B" }
      , node "G" KGroup Nothing [ "C" ]
      , (node "C" KTab (Just "G") []) { title = "C" }
      ]
  , byWindow = Map.fromFoldable [ Tuple 1 "W" ]
  , byTab = Map.fromFoldable [ Tuple 11 "A" ]
  }

spec :: Spec Unit
spec = describe "Model.View" do
  it "computeOrder tags each visible entry with its subtree end" do
    map (\o -> Tuple o.id o.subtreeEnd) (computeOrder "" m)
      `shouldEqual` [ Tuple "W" 3, Tuple "A" 2, Tuple "B" 3, Tuple "G" 5, Tuple "C" 5 ]

  it "sliceView windows the order, keeping absolute indices and subtree ends" do
    let rows = sliceView m (computeOrder "" m) 1 2
    map _.id rows `shouldEqual` [ "A", "B" ]
    map _.index rows `shouldEqual` [ 1, 2 ]
    map _.subtreeEnd rows `shouldEqual` [ 2, 3 ]
    map _.title rows `shouldEqual` [ "A", "B" ]
    (_.hasChildren <$> Array.head rows) `shouldEqual` Just false

  it "the first row carries its window/last-root flags" do
    let rows = sliceView m (computeOrder "" m) 0 1
    map _.hasChildren rows `shouldEqual` [ true ] -- W has children
    map _.isLastRoot rows `shouldEqual` [ false ] -- G is the last root

  it "focusIndexOf finds the window's active tab in the order" do
    focusIndexOf 1 (computeOrder "" m) m `shouldEqual` 1

  it "focusIndexOf is -1 when the window has no active tab" do
    focusIndexOf 99 (computeOrder "" m) m `shouldEqual` (-1)
