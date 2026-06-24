module Test.Model.GuideSpec where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Model.Guide (buildGuide, emptyGuide, guideBottom, guideFull, guideTop)
import Model.Tree (Entry)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

e :: String -> Int -> Entry
e id depth = { id, depth }

-- verticals as a literal: each (row, depth, flags) triple
vert :: Array (Tuple Int (Array (Tuple Int Int))) -> Map.Map Int (Map.Map Int Int)
vert = Map.fromFoldable <<< map (\(Tuple row segs) -> Tuple row (Map.fromFoldable segs))

horiz :: Array (Tuple Int Int) -> Map.Map Int Int
horiz = Map.fromFoldable

spec :: Spec Unit
spec = describe "Model.Guide" do
  describe "buildGuide" do
    it "connects a parent down to its two children (continuous vertical + stubs)" do
      -- window(0) -> [a(1), b(1)]
      let entries = [ e "w" 0, e "a" 1, e "b" 1 ]
      buildGuide entries 0 `shouldEqual`
        { verticals: vert
            [ Tuple 0 [ Tuple 1 guideBottom ] -- line opens at the parent
            , Tuple 1 [ Tuple 1 guideFull ] -- passes through the first child
            , Tuple 2 [ Tuple 1 guideTop ] -- ends at the last child
            ]
        , horizontals: horiz [ Tuple 1 1, Tuple 2 1 ]
        }

    it "traces a hovered leaf up to its parent only" do
      let entries = [ e "w" 0, e "a" 1, e "b" 1 ]
      buildGuide entries 1 `shouldEqual`
        { verticals: vert [ Tuple 0 [ Tuple 1 guideBottom ], Tuple 1 [ Tuple 1 guideTop ] ]
        , horizontals: horiz [ Tuple 1 1 ]
        }

    it "fills the gap created by a nested subtree between two siblings" do
      -- window(0) -> [a(1) -> [g(2)], b(1)]; the vertical at depth 1 must pass
      -- through g's row, and a gains a depth-2 vertical down to g
      let entries = [ e "w" 0, e "a" 1, e "g" 2, e "b" 1 ]
      buildGuide entries 0 `shouldEqual`
        { verticals: vert
            [ Tuple 0 [ Tuple 1 guideBottom ]
            , Tuple 1 [ Tuple 1 guideFull, Tuple 2 guideBottom ]
            , Tuple 2 [ Tuple 1 guideFull, Tuple 2 guideTop ]
            , Tuple 3 [ Tuple 1 guideTop ]
            ]
        , horizontals: horiz [ Tuple 1 1, Tuple 2 2, Tuple 3 1 ]
        }

    it "draws nothing for a top-level leaf (depth 0, no parent, no children)" do
      let entries = [ e "x" 0, e "y" 0 ]
      buildGuide entries 0 `shouldEqual` emptyGuide

    it "skips the guide past the subtree-size cap (bounded, no huge fan-out)" do
      -- 1001 children > maxGuideRows (1000): hovering the root yields no guide
      let entries = Array.cons (e "root" 0) (map (\i -> e ("c" <> show i) 1) (Array.range 1 1001))
      buildGuide entries 0 `shouldEqual` emptyGuide
