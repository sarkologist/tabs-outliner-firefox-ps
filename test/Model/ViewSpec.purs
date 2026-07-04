module Test.Model.ViewSpec where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Model.View (computeOrder, decodeView, encodeView, focusIndexOf, sliceView, startForView, viewStats)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

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
    let rows = sliceView m "" (computeOrder "" m) 1 2
    map _.id rows `shouldEqual` [ "A", "B" ]
    map _.index rows `shouldEqual` [ 1, 2 ]
    map _.subtreeEnd rows `shouldEqual` [ 2, 3 ]
    map _.title rows `shouldEqual` [ "A", "B" ]
    (_.hasChildren <$> Array.head rows) `shouldEqual` Just false

  it "the first row carries its window/last-root flags" do
    let rows = sliceView m "" (computeOrder "" m) 0 1
    map _.hasChildren rows `shouldEqual` [ true ] -- W has children
    map _.isLastRoot rows `shouldEqual` [ false ] -- G is the last root

  it "focusIndexOf finds the window's active tab in the order" do
    focusIndexOf 1 (computeOrder "" m) m `shouldEqual` 1

  it "focusIndexOf is -1 when the window has no active tab" do
    focusIndexOf 99 (computeOrder "" m) m `shouldEqual` (-1)

  it "marks only direct search matches, not ancestor path rows" do
    let rows = sliceView m "c" (computeOrder "c" m) 0 2
    map (\r -> Tuple r.id r.isSearchMatch) rows `shouldEqual` [ Tuple "G" false, Tuple "C" true ]

  it "centers a target row and clamps to the order bounds" do
    let order = computeOrder "" m
    startForView 0 3 (Just "C") order `shouldEqual` 2
    startForView 0 3 (Just "W") order `shouldEqual` 0
    startForView 4 3 Nothing order `shouldEqual` 2
    startForView 4 3 (Just "missing") order `shouldEqual` 2

  it "computes toolbar stats from the whole model" do
    viewStats "c" m `shouldEqual` { nodeTotal: 5, openTabTotal: 1, matchTotal: 1 }
    viewStats "" m `shouldEqual` { nodeTotal: 5, openTabTotal: 1, matchTotal: 0 }

  it "round-trips view stats on the wire" do
    let
      rows = sliceView m "" (computeOrder "" m) 0 1
      view = { total: 5, rows, focusIndex: 1, serverMs: 2.0, nodeTotal: 5, openTabTotal: 1, matchTotal: 0 }
    case decodeView (encodeView view) of
      Right decoded ->
        { nodeTotal: decoded.nodeTotal, openTabTotal: decoded.openTabTotal, matchTotal: decoded.matchTotal }
          `shouldEqual` { nodeTotal: 5, openTabTotal: 1, matchTotal: 0 }
      Left err -> fail err

  it "decodes old cached view JSON without stat fields" do
    case jsonParser """{"total":5,"rows":[],"focusIndex":-1,"serverMs":0}""" >>= decodeView of
      Right decoded ->
        { nodeTotal: decoded.nodeTotal, openTabTotal: decoded.openTabTotal, matchTotal: decoded.matchTotal }
          `shouldEqual` { nodeTotal: 5, openTabTotal: 0, matchTotal: 0 }
      Left err -> fail err
