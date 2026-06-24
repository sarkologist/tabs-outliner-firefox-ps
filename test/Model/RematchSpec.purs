module Test.Model.RematchSpec where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (Command(..), applyCommand)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Rematch (rematchOnStartup)
import Model.Types (Model, RuntimeTab, RuntimeWindow, Kind(..), emptyModel, isLive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (\m e -> (applyBrowser 0.0 e m).model) emptyModel

-- last session: window n1 (id 1) with tabs n2 (A, tab 11) and n3 (B, tab 12)
prior :: Model
prior = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false ]

rt :: Int -> Int -> Int -> String -> RuntimeTab
rt tabId windowId index title =
  { tabId, windowId, index, url: Just ("http://" <> title), title, active: false, favIconUrl: Nothing }

rw :: Int -> Array RuntimeTab -> RuntimeWindow
rw windowId tabs = { windowId, tabs }

rematch :: Array RuntimeWindow -> Model -> Model
rematch current m = (rematchOnStartup 1.0 current m).model

spec :: Spec Unit
spec = describe "Model.Rematch" do
  it "a clean restart re-binds existing nodes with fresh ids (no duplication)" do
    let m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "B" ] ] prior
    Map.size m.nodes `shouldEqual` 3
    m.roots `shouldEqual` [ "n1" ]
    (_.windowId <$> Map.lookup "n1" m.nodes) `shouldEqual` Just (Just 5)
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (_.tabId <$> Map.lookup "n3" m.nodes) `shouldEqual` Just (Just 52)
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    Map.lookup 51 m.byTab `shouldEqual` Just "n2"
    Map.lookup 5 m.byWindow `shouldEqual` Just "n1"

  it "a tab that did not reopen closes in place" do
    let m = rematch [ rw 5 [ rt 51 5 0 "A" ] ] prior
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3" ]

  it "a genuinely new tab gets a new node under its window" do
    let m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "B", rt 53 5 2 "C" ] ] prior
    Map.size m.nodes `shouldEqual` 4
    (_.kind <$> Map.lookup "n4" m.nodes) `shouldEqual` Just KTab
    (_.tabId <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just 53)
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3", "n4" ]

  it "a window that did not reopen closes as a restorable previous session" do
    let m = rematch [] prior
    (isLive <$> Map.lookup "n1" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just false
    (isLive <$> Map.lookup "n3" m.nodes) `shouldEqual` Just false
    m.roots `shouldEqual` [ "n1" ]

  it "duplicate urls across windows don't steal each other's nodes" do
    let
      prior2 = runEvents
        [ openTab 11 1 0 "A" true, openTab 12 1 1 "A2" true, openTab 13 1 2 "S" true
        , openTab 21 2 0 "B" true, openTab 22 2 1 "B2" true, openTab 23 2 2 "S" true
        ]
      -- n4 = window 1's "S" tab, n8 = window 2's "S" tab (same url http://S)
      m = rematch
        [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "A2", rt 53 5 2 "S" ]
        , rw 6 [ rt 61 6 0 "B", rt 62 6 1 "B2", rt 63 6 2 "S" ]
        ]
        prior2
    Map.size m.nodes `shouldEqual` 8 -- no duplication
    (_.tabId <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just 53) -- window 1's S stays in window 1
    (_.tabId <$> Map.lookup "n8" m.nodes) `shouldEqual` Just (Just 63) -- window 2's S stays in window 2

  it "preserves user organization: a reopened tab stays where it was moved" do
    let
      organized = (applyCommand 0.0 (Move "n2" (Just "n4") 0)
        (applyCommand 0.0 (NewGroup Nothing 0) prior).model).model -- A under a new group n4
      m = rematch [ rw 5 [ rt 51 5 0 "A", rt 52 5 1 "B" ] ] organized
    (_.parent <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just "n4") -- still under the group
    (isLive <$> Map.lookup "n2" m.nodes) `shouldEqual` Just true
    (_.tabId <$> Map.lookup "n2" m.nodes) `shouldEqual` Just (Just 51)
