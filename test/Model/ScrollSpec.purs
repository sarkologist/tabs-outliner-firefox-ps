module Test.Model.ScrollSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Model.Scroll (activeTabInWindow, revealScrollTop)
import Model.Tree (applyPatch)
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

win :: String -> Int -> Array String -> Node
win id wid children = (defaultNode id KWindow 0.0) { windowId = Just wid, children = children }

tab :: String -> Int -> Boolean -> Node
tab id tid active = (defaultNode id KTab 0.0) { tabId = Just tid, active = active }

grp :: String -> Array String -> Node
grp id children = (defaultNode id KGroup 0.0) { children = children }

-- Build a realistic model (indexes populated) from a node list + roots.
mk :: Array Node -> Array String -> Model
mk nodes roots = applyPatch { upserts: nodes, removes: [], roots: Just roots } emptyModel

-- window 1 -> [t1, t2*, t3]; window 2 -> [t4*]; window 3 -> [g -> [t5*]]
-- (* marks the active tab in each window)
fixture :: Model
fixture = mk
  [ win "w1" 1 [ "t1", "t2", "t3" ]
  , tab "t1" 11 false
  , tab "t2" 12 true
  , tab "t3" 13 false
  , win "w2" 2 [ "t4" ]
  , tab "t4" 24 true
  , win "w3" 3 [ "g" ]
  , grp "g" [ "t5" ]
  , tab "t5" 35 true
  ]
  [ "w1", "w2", "w3" ]

geom :: Number -> { rowHeight :: Number, viewportHeight :: Number, contentHeight :: Number, scrollTop :: Number }
geom scrollTop =
  -- 18px rows, a 10-row viewport, 100 rows of content
  { rowHeight: 18.0, viewportHeight: 180.0, contentHeight: 1800.0, scrollTop }

spec :: Spec Unit
spec = describe "Model.Scroll" do
  describe "activeTabInWindow" do
    it "finds the active tab in a window's direct children" do
      activeTabInWindow 1 fixture `shouldEqual` Just "t2"
    it "scopes to the asked window (each window has its own active tab)" do
      activeTabInWindow 2 fixture `shouldEqual` Just "t4"
    it "finds an active tab nested under a group inside the window" do
      activeTabInWindow 3 fixture `shouldEqual` Just "t5"
    it "returns Nothing for an unknown / non-live window" do
      activeTabInWindow 99 fixture `shouldEqual` Nothing
    it "returns Nothing when the window has no active tab" do
      let m = mk [ win "w" 1 [ "a" ], tab "a" 1 false ] [ "w" ]
      activeTabInWindow 1 m `shouldEqual` Nothing

  describe "revealScrollTop" do
    it "does not scroll a row already fully in view" do
      revealScrollTop (geom 0.0) 3 `shouldEqual` Nothing
    it "treats a partially clipped row as needing a scroll" do
      -- row 10 starts at 180px, just past a 180px viewport at top
      revealScrollTop (geom 0.0) 10 `shouldEqual` Just 99.0
    it "centers a row far below the fold" do
      -- 50*18 + 9 - 90 = 819
      revealScrollTop (geom 0.0) 50 `shouldEqual` Just 819.0
    it "clamps to the top for a row above an early-centered target" do
      revealScrollTop (geom 500.0) 2 `shouldEqual` Just 0.0
    it "clamps to the bottom for a row near the end" do
      -- centered 1701 clamps to contentHeight - viewportHeight = 1620
      revealScrollTop (geom 0.0) 99 `shouldEqual` Just 1620.0
