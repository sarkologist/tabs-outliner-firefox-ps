module Test.Model.ViewSpec where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (length) as String
import Data.String.Common (joinWith) as String
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

-- the same shape, with a page url + favicon on A and an absurdly large `data:`
-- favicon on B (the kind that must not ride along in every view window)
mIcons :: Model
mIcons = m
  { nodes = Map.union
      ( Map.fromFoldable $ map (\n -> Tuple n.id n)
          [ (node "A" KTab (Just "W") []) { tabId = Just 11, active = true, title = "A", url = Just "http://a/page", favIconUrl = Just "http://a/favicon.ico" }
          , (node "B" KTab (Just "W") []) { title = "B", url = Just "http://b/page", favIconUrl = Just hugeFavicon }
          ]
      )
      m.nodes
  }

-- a `data:` favicon of exactly `n` characters
favicon :: Int -> String
favicon n = prefix <> String.joinWith "" (Array.replicate (n - String.length prefix) "A")
  where
  prefix = "data:image/png;base64,"

-- the projected favicon of row A (index 1), given its icon and its binding
faviconOfRow :: String -> (Node -> Node) -> Array (Maybe String)
faviconOfRow url bind =
  map _.favIconUrl
    ( sliceView
        (m { nodes = Map.insert "A" (bind ((node "A" KTab (Just "W") []) { title = "A", favIconUrl = Just url })) m.nodes })
        ""
        (computeOrder "" m)
        1
        1
    )

hugeFavicon :: String
hugeFavicon = favicon 16385

-- one window of `n` tabs, each with a 16 KB favicon: what a tall full-size view
-- at minimum zoom asks for, and what the per-window budget has to survive
mManyIcons :: Int -> Model
mManyIcons n = emptyModel
  { roots = [ "W" ]
  , nodes = Map.fromFoldable $ map (\x -> Tuple x.id x)
      ( [ (node "W" KGroup Nothing (map tabId (Array.range 1 n))) { windowId = Just 1, title = "W" } ]
          <> map
            ( \i -> (node (tabId i) KTab (Just "W") []) { title = tabId i, favIconUrl = Just (favicon 16384) }
            )
            (Array.range 1 n)
      )
  , byWindow = Map.fromFoldable [ Tuple 1 "W" ]
  }
  where
  tabId i = "T" <> show i

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

  it "ignores stale byTab entries when computing open tab stats" do
    let
      stale =
        m
          { nodes = Map.insert "A" ((node "A" KTab (Just "W") []) { title = "A" }) m.nodes
          , byTab = Map.fromFoldable [ Tuple 11 "A", Tuple 12 "missing" ]
          }
    viewStats "" stale `shouldEqual` { nodeTotal: 5, openTabTotal: 0, matchTotal: 0 }

  it "projects each row's url and favicon" do
    let rows = sliceView mIcons "" (computeOrder "" mIcons) 1 1
    map _.url rows `shouldEqual` [ Just "http://a/page" ]
    map _.favIconUrl rows `shouldEqual` [ Just "http://a/favicon.ico" ]

  it "drops a favicon too large to ride in every view window" do
    -- the url still travels (it is small, and it is the row's tooltip); the row
    -- falls back to its kind glyph instead of carrying ~5 KB of base64
    let rows = sliceView mIcons "" (computeOrder "" mIcons) 2 1
    map _.url rows `shouldEqual` [ Just "http://b/page" ]
    map _.favIconUrl rows `shouldEqual` [ Nothing ]

  it "keeps a favicon at the per-icon limit and drops the one past it" do
    let
      one m' = sliceView m' "" (computeOrder "" m') 1 1
      with url = m { nodes = Map.insert "A" ((node "A" KTab (Just "W") []) { title = "A", favIconUrl = Just url }) m.nodes }
    map _.favIconUrl (one (with (favicon 16384))) `shouldEqual` [ Just (favicon 16384) ]
    map _.favIconUrl (one (with (favicon 16385))) `shouldEqual` [ Nothing ]

  it "renders only favicon schemes an <img> may be pointed at" do
    let
      -- A is the live tab (tab 11)
      faviconOf url = faviconOfRow url (\n -> n { tabId = Just 11 })
    faviconOf "https://a/i.png" `shouldEqual` [ Just "https://a/i.png" ]
    faviconOf "data:image/png;base64,AAAA" `shouldEqual` [ Just "data:image/png;base64,AAAA" ]
    faviconOf "javascript:alert(1)" `shouldEqual` [ Nothing ]
    faviconOf "chrome://branding/content/icon32.png" `shouldEqual` [ Nothing ]
    faviconOf "data:text/html,<script>x</script>" `shouldEqual` [ Nothing ]

  it "renders a closed row's icon from stored bytes only, never a url" do
    let
      -- the same row, closed: a page the user is not on
      faviconOf url = faviconOfRow url identity
    faviconOf "data:image/png;base64,AAAA" `shouldEqual` [ Just "data:image/png;base64,AAAA" ]
    -- a url would be a request to that site — the leak (and, for an imported
    -- tree, the beacon) this rule exists to close
    faviconOf "https://a/i.png" `shouldEqual` [ Nothing ]
    faviconOf "http://beacon.example/n1.png" `shouldEqual` [ Nothing ]
    faviconOf "moz-extension://x/icon.png" `shouldEqual` [ Nothing ]

  it "a live window is not a tab: it draws stored bytes only" do
    -- W (row 0) is live window 1; no tab reports a window's icon, so any url on
    -- it is one a file claimed
    let
      windowIcon url =
        map _.favIconUrl
          ( sliceView
              (m { nodes = Map.update (\w -> Just (w { favIconUrl = Just url })) "W" m.nodes })
              ""
              (computeOrder "" m)
              0
              1
          )
    windowIcon "https://beacon.example/w.png" `shouldEqual` [ Nothing ]
    windowIcon "data:image/png;base64,AAAA" `shouldEqual` [ Just "data:image/png;base64,AAAA" ]

  it "spends a favicon budget across the whole window, not per row" do
    let
      big = mManyIcons 200
      rows = sliceView big "" (computeOrder "" big) 1 200
      withIcons = Array.length (Array.filter (\r -> r.favIconUrl /= Nothing) rows)
    -- 131072 / 16384 = 8 rows keep their icon; the rest fall back to a glyph but
    -- are otherwise intact, so the reply stays bounded however tall the viewport
    withIcons `shouldEqual` 8
    Array.length rows `shouldEqual` 200
    map _.title (Array.take 1 (Array.drop 8 rows)) `shouldEqual` [ "T9" ]

  it "clamps a url to a length a tooltip can use" do
    let
      long = "http://a/?q=" <> String.joinWith "" (Array.replicate 4000 "x")
      rows = sliceView (m { nodes = Map.insert "A" ((node "A" KTab (Just "W") []) { title = "A", url = Just long }) m.nodes }) "" (computeOrder "" m) 1 1
    map (\r -> String.length <$> r.url) rows `shouldEqual` [ Just 2049 ] -- 2048 + the ellipsis

  it "round-trips a row's url and favicon on the wire" do
    let
      rows = sliceView mIcons "" (computeOrder "" mIcons) 1 1
      view = { total: 5, rows, focusIndex: 1, serverMs: 0.0, nodeTotal: 5, openTabTotal: 1, matchTotal: 0 }
    case decodeView (encodeView view) of
      Right decoded ->
        map (\r -> Tuple r.url r.favIconUrl) decoded.rows
          `shouldEqual` [ Tuple (Just "http://a/page") (Just "http://a/favicon.ico") ]
      Left err -> fail err

  it "decodes a cached row saved before rows carried a url or favicon" do
    let
      json = """{"total":1,"focusIndex":-1,"serverMs":0,"rows":[{"id":"A","index":0,"depth":0,"subtreeEnd":1,"kind":"tab","title":"A","live":false,"active":false,"collapsed":false,"hasChildren":false,"isLastRoot":true}]}"""
    case jsonParser json >>= decodeView of
      Right decoded ->
        map (\r -> Tuple r.url r.favIconUrl) decoded.rows `shouldEqual` [ Tuple Nothing Nothing ]
      Left err -> fail err

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
