module Test.Model.ViewSpec where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length) as String
import Data.String.Common (joinWith) as String
import Data.Tuple (Tuple(..))
import Model.Types (Kind(..), Model, Node, defaultNode, emptyModel)
import Model.View (computeOrder, decodeView, encodeView, focusIndexOf, originIcons, sliceView, startForView, viewStats)
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
        Map.empty
        ""
        (computeOrder "" m)
        1
        1
    )

hugeFavicon :: String
hugeFavicon = favicon 16385

-- a `data:` favicon of exactly `n` characters, distinct for each `i`
distinctFavicon :: Int -> Int -> String
distinctFavicon i n = prefix <> String.joinWith "" (Array.replicate (n - String.length prefix) "A")
  where
  prefix = "data:image/png;base64," <> show i <> "/"

-- one window of `n` tabs, each with its own 16 KB favicon: what a tall full-size
-- view at minimum zoom asks for, and what the per-window budget has to survive
mManyIcons :: Int -> Model
mManyIcons n = mIconsBy n (\i -> distinctFavicon i 16384)

-- one window of `n` tabs, tab `i` carrying `iconOf i`
mIconsBy :: Int -> (Int -> String) -> Model
mIconsBy n iconOf = emptyModel
  { roots = [ "W" ]
  , nodes = Map.fromFoldable $ map (\x -> Tuple x.id x)
      ( [ (node "W" KGroup Nothing (map tabId (Array.range 1 n))) { windowId = Just 1, title = "W" } ]
          <> map
            ( \i -> (node (tabId i) KTab (Just "W") []) { title = tabId i, favIconUrl = Just (iconOf i) }
            )
            (Array.range 1 n)
      )
  , byWindow = Map.fromFoldable [ Tuple 1 "W" ]
  }
  where
  tabId i = "T" <> show i

-- one saved group "W" of closed tabs, tab `i` at `sites[i-1]`'s url and icon
mSites :: Array { url :: String, icon :: Maybe String } -> Model
mSites sites = emptyModel
  { roots = [ "W" ]
  , nodes = Map.fromFoldable $ map (\x -> Tuple x.id x)
      ( [ (node "W" KGroup Nothing (map tabId (Array.range 1 (Array.length sites)))) { title = "W" } ]
          <> Array.mapWithIndex
            (\i site -> (node (tabId (i + 1)) KTab (Just "W") []) { title = tabId (i + 1), url = Just site.url, favIconUrl = site.icon })
            sites
      )
  }
  where
  tabId i = "T" <> show i

spec :: Spec Unit
spec = describe "Model.View" do
  it "computeOrder tags each visible entry with its subtree end" do
    map (\o -> Tuple o.id o.subtreeEnd) (computeOrder "" m)
      `shouldEqual` [ Tuple "W" 3, Tuple "A" 2, Tuple "B" 3, Tuple "G" 5, Tuple "C" 5 ]

  it "sliceView windows the order, keeping absolute indices and subtree ends" do
    let rows = sliceView m Map.empty "" (computeOrder "" m) 1 2
    map _.id rows `shouldEqual` [ "A", "B" ]
    map _.index rows `shouldEqual` [ 1, 2 ]
    map _.subtreeEnd rows `shouldEqual` [ 2, 3 ]
    map _.title rows `shouldEqual` [ "A", "B" ]
    (_.hasChildren <$> Array.head rows) `shouldEqual` Just false

  it "the first row carries its window/last-root flags" do
    let rows = sliceView m Map.empty "" (computeOrder "" m) 0 1
    map _.hasChildren rows `shouldEqual` [ true ] -- W has children
    map _.isLastRoot rows `shouldEqual` [ false ] -- G is the last root

  it "focusIndexOf finds the window's active tab in the order" do
    focusIndexOf 1 (computeOrder "" m) m `shouldEqual` 1

  it "focusIndexOf is -1 when the window has no active tab" do
    focusIndexOf 99 (computeOrder "" m) m `shouldEqual` (-1)

  it "marks only direct search matches, not ancestor path rows" do
    let rows = sliceView m Map.empty "c" (computeOrder "c" m) 0 2
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
    let rows = sliceView mIcons Map.empty "" (computeOrder "" mIcons) 1 1
    map _.url rows `shouldEqual` [ Just "http://a/page" ]
    map _.favIconUrl rows `shouldEqual` [ Just "http://a/favicon.ico" ]

  it "drops a favicon too large to ride in every view window" do
    -- the url still travels (it is small, and it is the row's tooltip); the row
    -- falls back to its kind glyph instead of carrying ~5 KB of base64
    let rows = sliceView mIcons Map.empty "" (computeOrder "" mIcons) 2 1
    map _.url rows `shouldEqual` [ Just "http://b/page" ]
    map _.favIconUrl rows `shouldEqual` [ Nothing ]

  it "keeps a favicon at the per-icon limit and drops the one past it" do
    let
      one m' = sliceView m' Map.empty "" (computeOrder "" m') 1 1
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
              Map.empty
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
      rows = sliceView big Map.empty "" (computeOrder "" big) 1 200
      withIcons = Array.length (Array.filter (\r -> r.favIconUrl /= Nothing) rows)
    -- 131072 / 16384 = 8 rows keep their icon; the rest fall back to a glyph but
    -- are otherwise intact, so the reply stays bounded however tall the viewport
    withIcons `shouldEqual` 8
    Array.length rows `shouldEqual` 200
    map _.title (Array.take 1 (Array.drop 8 rows)) `shouldEqual` [ "T9" ]

  it "charges a repeated icon once, so a column of one site keeps every icon" do
    let
      -- a column of YouTube rows is one icon over and over: per-row charging let
      -- ~40 copies of it spend the budget and blanked every row below them
      same = mIconsBy 200 (const (favicon 16384))
      rows = sliceView same Map.empty "" (computeOrder "" same) 1 200
    Array.length (Array.filter (\r -> r.favIconUrl == Just (favicon 16384)) rows) `shouldEqual` 200

  it "lets a repeat of an admitted icon ride free once the budget is spent" do
    let
      -- eight distinct 16 KB icons spend all 131072; row 9 repeats one of them
      -- (free), row 10 is a ninth distinct one (dropped)
      mixed = mIconsBy 10 (\i -> if i == 9 then distinctFavicon 1 16384 else distinctFavicon i 16384)
      rows = sliceView mixed Map.empty "" (computeOrder "" mixed) 1 10
    map (\r -> r.favIconUrl /= Nothing) rows `shouldEqual` (Array.replicate 9 true <> [ false ])

  it "points each row at its own icon in the wire table" do
    let
      a = distinctFavicon 1 64
      b = distinctFavicon 2 64
      dropped = hugeFavicon -- past the per-icon cap: never rendered
      icons = [ Just a, Just dropped, Just b, Just a, Nothing, Just b ]
      model = mIconsBy 6 (\i -> fromMaybe "" (join (Array.index icons (i - 1))))
      rows = sliceView model Map.empty "" (computeOrder "" model) 1 6
      view = { total: 7, rows, focusIndex: -1, serverMs: 0.0, nodeTotal: 7, openTabTotal: 0, matchTotal: 0 }
      wire = encodeView view
      wireShape = decodeJson wire :: Either _ { icons :: Array String, rows :: Array { icon :: Maybe Int } }
    map (\w -> { icons: w.icons, slots: map _.icon w.rows }) wireShape
      `shouldEqual` Right { icons: [ a, b ], slots: [ Just 0, Nothing, Just 1, Just 0, Nothing, Just 1 ] }
    case decodeView wire of
      Right decoded -> map _.favIconUrl decoded.rows `shouldEqual` [ Just a, Nothing, Just b, Just a, Nothing, Just b ]
      Left err -> fail err

  it "sends a repeated icon over the wire once" do
    let
      same = mIconsBy 50 (const (favicon 4096))
      rows = sliceView same Map.empty "" (computeOrder "" same) 1 50
      view = { total: 51, rows, focusIndex: -1, serverMs: 0.0, nodeTotal: 51, openTabTotal: 0, matchTotal: 0 }
      wire = encodeView view
      bare = encodeView view { rows = map (_ { favIconUrl = Nothing }) rows }
    -- 50 rows, one 4 KB icon: the icons add it once (plus a slot index per row),
    -- not 50 times
    (String.length (stringify wire) - String.length (stringify bare) < 2 * 4096) `shouldEqual` true
    case decodeView wire of
      Right decoded -> map _.favIconUrl decoded.rows `shouldEqual` Array.replicate 50 (Just (favicon 4096))
      Left err -> fail err

  it "lends an icon-less tab its origin's stored icon" do
    let
      yt = distinctFavicon 1 3450
      -- T1 carries YouTube's icon; T2 (same origin, none captured) borrows it,
      -- T3 (another origin) and T4 (http, not https) do not
      model = mSites
        [ { url: "https://www.youtube.com/watch?v=1", icon: Just yt }
        , { url: "https://www.youtube.com/watch?v=2", icon: Nothing }
        , { url: "https://example.com/", icon: Nothing }
        , { url: "http://www.youtube.com/watch?v=3", icon: Nothing }
        ]
      rows = sliceView model (originIcons model) "" (computeOrder "" model) 0 5
    map _.favIconUrl rows `shouldEqual` [ Nothing, Just yt, Just yt, Nothing, Nothing ]

  it "keeps a tab's own icon over its origin's" do
    let
      a = distinctFavicon 1 100
      b = distinctFavicon 2 100
      model = mSites [ { url: "https://a.example/1", icon: Just a }, { url: "https://a.example/1", icon: Just a }, { url: "https://a.example/2", icon: Just b } ]
      rows = sliceView model (originIcons model) "" (computeOrder "" model) 1 3
    map _.favIconUrl rows `shouldEqual` [ Just a, Just a, Just b ]

  it "shares the icon most of an origin's tabs carry, ties to the smaller" do
    let
      usual = distinctFavicon 1 3450
      badge = distinctFavicon 2 666 -- an unread-count variant, on one tab
      small = distinctFavicon 3 500
      big = distinctFavicon 4 900
      shared sites = Map.lookup "https://a.example" (originIcons (mSites sites))
    shared
      [ { url: "https://a.example/1", icon: Just usual }
      , { url: "https://a.example/2", icon: Just badge }
      , { url: "https://a.example/3", icon: Just usual }
      ] `shouldEqual` Just usual
    shared [ { url: "https://a.example/1", icon: Just big }, { url: "https://a.example/2", icon: Just small } ]
      `shouldEqual` Just small

  it "shares only stored bytes, never an icon url" do
    let
      -- a live tab may draw the url its page reports, but lending that url to a
      -- closed row would be a request to the site the user is not on
      model = mSites [ { url: "https://a.example/1", icon: Just "https://a.example/favicon.ico" } ]
    originIcons model `shouldEqual` Map.empty

  it "never lends a window or group an icon" do
    let
      yt = distinctFavicon 1 100
      model = mSites [ { url: "https://a.example/1", icon: Just yt } ]
      withGroupUrl = model { nodes = Map.update (\w -> Just (w { url = Just "https://a.example/" })) "W" model.nodes }
      rows = sliceView withGroupUrl (originIcons withGroupUrl) "" (computeOrder "" withGroupUrl) 0 1
    map _.favIconUrl rows `shouldEqual` [ Nothing ]

  it "clamps a url to a length a tooltip can use" do
    let
      long = "http://a/?q=" <> String.joinWith "" (Array.replicate 4000 "x")
      rows = sliceView (m { nodes = Map.insert "A" ((node "A" KTab (Just "W") []) { title = "A", url = Just long }) m.nodes }) Map.empty "" (computeOrder "" m) 1 1
    map (\r -> String.length <$> r.url) rows `shouldEqual` [ Just 2049 ] -- 2048 + the ellipsis

  it "round-trips a row's url and favicon on the wire" do
    let
      rows = sliceView mIcons Map.empty "" (computeOrder "" mIcons) 1 1
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
      rows = sliceView m Map.empty "" (computeOrder "" m) 0 1
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
