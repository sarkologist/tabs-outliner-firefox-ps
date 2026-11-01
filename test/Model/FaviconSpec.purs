module Test.Model.FaviconSpec where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (length) as String
import Data.String.Common (joinWith) as String
import Model.Event (BrowserEvent(..))
import Model.Favicon (importedIcon, isStorableIcon, mergeReported, needsShrink, origin, reportedIcon, sameSite, storedIconByteLimit)
import Model.Reconcile (applyBrowser)
import Model.Rematch (rematchOnStartup)
import Model.Types (Model, emptyModel, isLive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- a `data:` png url of exactly `n` characters
icon :: Int -> String
icon n = prefix <> String.joinWith "" (Array.replicate (n - String.length prefix) "A")
  where
  prefix = "data:image/png;base64,"

small :: String
small = icon 100

other :: String
other = icon 200

atCap :: String
atCap = icon storedIconByteLimit

overCap :: String
overCap = icon (storedIconByteLimit + 1)

remote :: String
remote = "https://a.example/favicon.ico"

opened :: Int -> Maybe String -> BrowserEvent
opened tabId favIconUrl =
  TabOpened { tabId, windowId: 1, openerTabId: Nothing, index: 0, url: Just "http://a", title: "A", active: true, favIconUrl }

changed :: Int -> Maybe String -> BrowserEvent
changed tabId favIconUrl = TabChanged { tabId, url: Nothing, title: Nothing, favIconUrl }

run :: Array BrowserEvent -> Model -> Model
run evs m0 = foldl (\m e -> (applyBrowser 0.0 e m).model) m0 evs

-- the stored icon of the (only) tab node, n2 (n1 is its window)
iconOf :: Model -> Maybe (Maybe String)
iconOf m = _.favIconUrl <$> Map.lookup "n2" m.nodes

spec :: Spec Unit
spec = describe "Model.Favicon" do
  it "stores bytes up to the cap, and shrinks (not stores) anything past it" do
    isStorableIcon atCap `shouldEqual` true
    needsShrink atCap `shouldEqual` false
    isStorableIcon overCap `shouldEqual` false
    needsShrink overCap `shouldEqual` true
    -- a url is not bytes: never stored as an icon, never shrunk (nothing fetches it)
    isStorableIcon remote `shouldEqual` false
    needsShrink remote `shouldEqual` false
    needsShrink ("data:text/html," <> String.joinWith "" (Array.replicate 5000 "x")) `shouldEqual` false

  it "merges what a live tab reports into what the node keeps" do
    -- silence keeps the old icon; storable bytes replace it
    mergeReported (Just small) Nothing `shouldEqual` Just small
    mergeReported (Just small) (Just other) `shouldEqual` Just other
    mergeReported (Just remote) (Just small) `shouldEqual` Just small
    -- over-cap bytes wait for their shrunk copy rather than wiping the icon
    mergeReported (Just small) (Just overCap) `shouldEqual` Just small
    mergeReported Nothing (Just overCap) `shouldEqual` Nothing
    -- a url is kept for the live row, but never displaces stored bytes
    mergeReported Nothing (Just remote) `shouldEqual` Just remote
    mergeReported (Just small) (Just remote) `shouldEqual` Just small
    -- an empty url is the tab clearing its icon
    mergeReported (Just small) (Just "") `shouldEqual` Nothing

  it "tells whether two page urls are on the same site" do
    sameSite (Just "https://a.example/x") (Just "https://a.example/y?q#f") `shouldEqual` true
    sameSite (Just "https://a.example/x") (Just "https://b.example/x") `shouldEqual` false
    sameSite (Just "https://a.example") (Just "https://a.example?x") `shouldEqual` true
    sameSite (Just "https://a.example/") (Just "about:addons") `shouldEqual` false
    sameSite Nothing (Just "https://a.example/") `shouldEqual` false

  it "takes a web page url's origin: scheme, host and port" do
    origin "https://www.YouTube.com/watch?v=x#t=1" `shouldEqual` Just "https://www.youtube.com"
    origin "http://localhost:8080/a?b" `shouldEqual` Just "http://localhost:8080"
    origin "https://a.example?x" `shouldEqual` Just "https://a.example"
    -- scheme is part of it: an http page does not share an https page's icon
    (origin "http://a.example/" == origin "https://a.example/") `shouldEqual` false
    origin "about:addons" `shouldEqual` Nothing
    origin "file:///Users/x/page.html" `shouldEqual` Nothing
    origin "moz-extension://abc/page.html" `shouldEqual` Nothing
    origin "https://" `shouldEqual` Nothing
    -- a default port is the same origin; another port is not
    origin "https://a.example:443/x" `shouldEqual` Just "https://a.example"
    origin "http://a.example:80/" `shouldEqual` Just "http://a.example"
    origin "https://a.example:8443/" `shouldEqual` Just "https://a.example:8443"
    origin "http://[::1]:8080/x" `shouldEqual` Just "http://[::1]:8080"
    -- userinfo stays in: such a url only shares with itself, never with the host
    -- (or the host it pretends to be)
    (origin "https://a.example@b.example/" == origin "https://b.example/") `shouldEqual` false
    (origin "https://a.example@b.example/" == origin "https://a.example/") `shouldEqual` false

  it "keeps only storable bytes of an imported icon" do
    importedIcon (Just small) `shouldEqual` Just small
    importedIcon (Just atCap) `shouldEqual` Just atCap
    importedIcon (Just overCap) `shouldEqual` Nothing
    importedIcon (Just remote) `shouldEqual` Nothing
    importedIcon (Just "http://beacon.example/n1.png") `shouldEqual` Nothing
    importedIcon Nothing `shouldEqual` Nothing

  it "finds the icon an event reports" do
    reportedIcon (opened 11 (Just small)) `shouldEqual` Just { tabId: 11, icon: small }
    reportedIcon (changed 11 (Just overCap)) `shouldEqual` Just { tabId: 11, icon: overCap }
    reportedIcon (changed 11 Nothing) `shouldEqual` Nothing
    reportedIcon (TabClosed { tabId: 11 }) `shouldEqual` Nothing

  describe "capture in the reducer" do
    it "stores a live tab's small icon bytes as they are" do
      iconOf (run [ opened 11 (Just small) ] emptyModel) `shouldEqual` Just (Just small)

    it "does not store an over-cap icon, then takes its shrunk copy" do
      let m = run [ opened 11 (Just overCap) ] emptyModel
      iconOf m `shouldEqual` Just Nothing
      -- the background's shrink lands as an ordinary TabChanged
      iconOf (run [ changed 11 (Just small) ] m) `shouldEqual` Just (Just small)

    it "keeps the current icon while a newer over-cap one is being shrunk" do
      iconOf (run [ opened 11 (Just small), changed 11 (Just overCap) ] emptyModel) `shouldEqual` Just (Just small)

    it "a reported url never displaces stored bytes" do
      iconOf (run [ opened 11 (Just small), changed 11 (Just remote) ] emptyModel) `shouldEqual` Just (Just small)

    it "navigating to another site drops the old site's icon" do
      let
        nav url = TabChanged { tabId: 11, url: Just url, title: Nothing, favIconUrl: Nothing }
        live = run [ opened 11 (Just small) ] emptyModel
      iconOf (run [ nav "http://a/other" ] live) `shouldEqual` Just (Just small)
      iconOf (run [ nav "https://b.example/" ] live) `shouldEqual` Just Nothing
      -- the new page's own icon, in the same event, is kept
      iconOf (run [ TabChanged { tabId: 11, url: Just "https://b.example/", title: Nothing, favIconUrl: Just other } ] live)
        `shouldEqual` Just (Just other)
      -- the stale bytes are not protected from the new site's url either
      iconOf (run [ nav "https://b.example/", changed 11 (Just remote) ] live) `shouldEqual` Just (Just remote)

    it "an event that changes nothing writes nothing" do
      let live = run [ opened 11 (Just small) ] emptyModel
      (applyBrowser 0.0 (changed 11 (Just small)) live).patch.upserts `shouldEqual` []
      Array.length (applyBrowser 0.0 (changed 11 (Just other)) live).patch.upserts `shouldEqual` 1

    it "a stored url is not carried over when its tab is re-found" do
      let
        prior = run [ opened 11 (Just remote) ] emptyModel
        rt = { tabId: 21, windowId: 2, openerTabId: Nothing, index: 0, url: Just "http://a", title: "A", active: true, favIconUrl: Nothing, nodeKey: Just "n2" }
      iconOf ((rematchOnStartup 1.0 [ { windowId: 2, tabs: [ rt ] } ] prior).model) `shouldEqual` Just Nothing

    it "a closed tab keeps its stored bytes" do
      let
        live = run [ opened 11 (Just small) ] emptyModel
        -- an outliner close ("save & close") keeps the row as closed history
        closed = run [ TabClosed { tabId: 11 } ] (live { closingTabs = Set.insert 11 live.closingTabs })
      (isLive <$> Map.lookup "n2" closed.nodes) `shouldEqual` Just false
      iconOf closed `shouldEqual` Just (Just small)

    it "a tab re-found at startup keeps its stored icon until it reports one" do
      let
        prior = run [ opened 11 (Just small) ] emptyModel
        rt favIconUrl = { tabId: 21, windowId: 2, openerTabId: Nothing, index: 0, url: Just "http://a", title: "A", active: true, favIconUrl, nodeKey: Just "n2" }
        rematched favIconUrl = (rematchOnStartup 1.0 [ { windowId: 2, tabs: [ rt favIconUrl ] } ] prior).model
      iconOf (rematched Nothing) `shouldEqual` Just (Just small)
      iconOf (rematched (Just other)) `shouldEqual` Just (Just other)
      iconOf (rematched (Just overCap)) `shouldEqual` Just (Just small)
