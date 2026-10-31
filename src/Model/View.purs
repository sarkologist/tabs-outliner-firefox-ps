-- | The projection: how the background turns its model into the windowed `View`
-- | the sidebar renders. The sidebar never holds the model — it asks for a slice
-- | of the visible order and gets back exactly the rows in view. Pure (so it is
-- | unit-tested without a browser); the background owns the order cache around it.
module Model.View
  ( ViewRow
  , View
  , ViewStats
  , OrderEntry
  , computeOrder
  , sliceView
  , startForView
  , focusIndexOf
  , viewStats
  , encodeView
  , decodeView
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (length, stripPrefix, take) as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (mapAccumL)
import Model.Codec (kindStr, parseKind)
import Model.Favicon (isIconBytes)
import Model.Search (matchesSearch, normalizeSearchQuery)
import Model.Scroll (activeTabInWindow)
import Model.Tree (Entry, searchVisible, visible)
import Model.Types (Kind(..), Model, NodeId, displayTitle, isLive)

-- A visible-order entry tagged with the flat index just past its subtree (used by
-- the drop preview to land a drop after a collapsed/expanded group's whole span).
type OrderEntry = { id :: NodeId, depth :: Int, subtreeEnd :: Int }

-- One projected row: everything a sidebar needs to render it and treat it as a
-- command / drag target, with no access to the model.
type ViewRow =
  { id :: NodeId
  , index :: Int
  , depth :: Int
  , subtreeEnd :: Int
  , kind :: Kind
  , title :: String
  , url :: Maybe String
  , favIconUrl :: Maybe String
  , live :: Boolean
  , active :: Boolean
  , collapsed :: Boolean
  , hasChildren :: Boolean
  , isLastRoot :: Boolean
  , isSearchMatch :: Boolean
  }

-- `serverMs` is the background's own compute time for this window (0 unless
-- profiling), so the sidebar can split the round-trip into compute vs transport.
type View =
  { total :: Int
  , rows :: Array ViewRow
  , focusIndex :: Int
  , serverMs :: Number
  , nodeTotal :: Int
  , openTabTotal :: Int
  , matchTotal :: Int
  }

type ViewStats = { nodeTotal :: Int, openTabTotal :: Int, matchTotal :: Int }

-- | The full visible order for a query, each entry tagged with its subtree end.
-- | `visible`/`searchVisible` are reused as-is; `withSubtreeEnds` is one O(N) pass.
computeOrder :: String -> Model -> Array OrderEntry
computeOrder query model =
  let q = normalizeSearchQuery query
  in withSubtreeEnds (if q == "" then visible model else searchVisible q model)

-- subtreeEnd[i] = first j>i whose depth <= depth[i] (else n). One left-to-right
-- pass over a depth-monotone stack: arriving at depth d closes every still-open
-- entry of depth >= d (its subtree ended at the current row).
withSubtreeEnds :: Array Entry -> Array OrderEntry
withSubtreeEnds entries =
  let
    n = Array.length entries
    final = foldlWithIndex step { stack: Nil, ends: Map.empty } entries
    ends = foldl (\m s -> Map.insert s.idx n m) final.ends final.stack
  in
    Array.mapWithIndex
      (\i e -> { id: e.id, depth: e.depth, subtreeEnd: fromMaybe n (Map.lookup i ends) })
      entries
  where
  step i acc e =
    let
      r = List.span (\s -> s.depth >= e.depth) acc.stack
    in
      { stack: Cons { idx: i, depth: e.depth } r.rest
      , ends: foldl (\m s -> Map.insert s.idx i m) acc.ends r.init
      }

-- | Project the rows in window `[start, start+count)` of an already-computed order.
sliceView :: Model -> String -> Array OrderEntry -> Int -> Int -> Array ViewRow
sliceView model query order start count =
  let
    total = Array.length order
    -- clamp to the last legal window, so a start past a shrunk order (collapse/
    -- delete while scrolled near the bottom) shows the tail, not a blank window
    s = clamp 0 (max 0 (total - count)) start
    lastRoot = Array.last model.roots
    window = Array.slice s (s + count) order
  in
    budgetFavicons
      (Array.catMaybes (Array.mapWithIndex (\off oe -> toRow model query lastRoot (s + off) oe) window))

-- | Resolve the start row for a view request. A target-centered request wins
-- | over the caller's scroll start when the target exists in the current order.
startForView :: Int -> Int -> Maybe NodeId -> Array OrderEntry -> Int
startForView requested count targetNodeId order =
  let
    total = Array.length order
    maxStart = max 0 (total - count)
    centered i = i - div count 2
  in
    case targetNodeId >>= \target -> Array.findIndex (\o -> o.id == target) order of
      Just i -> clamp 0 maxStart (centered i)
      Nothing -> clamp 0 maxStart requested

-- Keeping the reply bounded ---------------------------------------------------
--
-- Favicons and urls travel inline in every view window, and `data:` favicons are
-- what make a real tree heavy (4.5 MB of one 13.4 MB tree). The failure they
-- threaten is not a loud one: an over-large `runtime.sendMessage` *response*
-- resolves as `undefined` instead of rejecting, so the sidebar would simply stop
-- painting (the bug Export hit). The window is NOT ~50 rows — the full-size view
-- at minimum zoom asks for a row per 10.8px, so a tall display can ask for ~200 —
-- hence a budget over the whole window, not just a per-row test.

-- | Per icon: enough for a multi-resolution `.ico` or an SVG (what Firefox
-- | actually hands back), past which it is not a 16px image any more.
faviconByteLimit :: Int
faviconByteLimit = 16384

-- | Per window, across all its rows: the first rows that fit keep their icon and
-- | the rest fall back to their kind glyph. Chosen so a window's favicons stay an
-- | order of magnitude below the ~1.3 MB at which a reply was seen to vanish,
-- | leaving room for the titles and urls beside them.
faviconWindowByteBudget :: Int
faviconWindowByteBudget = 131072

-- | A url is a tooltip here, and no one reads 2 KB of one; capping it keeps a
-- | tall window's urls bounded too (a share/state url can run to tens of KB).
urlByteLimit :: Int
urlByteLimit = 2048

-- | What an `<img>` in the sidebar may be pointed at. A CLOSED row gets icon
-- | bytes only: anything else would be a request to a site the user is not on
-- | (see `Model.Favicon`). A live row may also point at the url its tab reports —
-- | that tab is already talking to that host — gated to the schemes an image can
-- | load from, the same kind of gate `Model.Command`'s `blockedSchemes` is for
-- | urls: nothing exotic reaches the DOM, and an `about:`/`chrome:` icon the
-- | sidebar could not load anyway falls back to the kind glyph.
renderableFavicon :: Boolean -> String -> Maybe String
renderableFavicon live u
  | String.length u > faviconByteLimit = Nothing
  | isIconBytes u = Just u
  | live && Array.any (\scheme -> isJust (String.stripPrefix (Pattern scheme) u)) [ "https://", "http://", "moz-extension://" ] = Just u
  | otherwise = Nothing

clampUrl :: String -> String
clampUrl u = if String.length u <= urlByteLimit then u else String.take urlByteLimit u <> "\x2026"

-- | Spend `faviconWindowByteBudget` left to right over a projected window: rows
-- | past it keep everything but their icon.
budgetFavicons :: Array ViewRow -> Array ViewRow
budgetFavicons rows = (mapAccumL spend faviconWindowByteBudget rows).value
  where
  spend left r = case r.favIconUrl of
    Just u
      | String.length u <= left -> { accum: left - String.length u, value: r }
      | otherwise -> { accum: left, value: r { favIconUrl = Nothing } }
    Nothing -> { accum: left, value: r }

toRow :: Model -> String -> Maybe NodeId -> Int -> OrderEntry -> Maybe ViewRow
toRow model query lastRoot i oe = Map.lookup oe.id model.nodes <#> \n ->
  let live = isLive n in
  { id: n.id
  , index: i
  , depth: oe.depth
  , subtreeEnd: oe.subtreeEnd
  , kind: n.kind
  , title: displayTitle n
  , url: clampUrl <$> n.url
  , favIconUrl: n.favIconUrl >>= renderableFavicon live
  , live
  , active: n.active
  , collapsed: n.collapsed
  , hasChildren: not (Array.null n.children)
  , isLastRoot: Just n.id == lastRoot
  , isSearchMatch: matchesSearch query n
  }

-- | Flat index of this window's active tab in the order, or -1. O(order), so the
-- | background only computes it when the sidebar asks (open / focus change).
focusIndexOf :: Int -> Array OrderEntry -> Model -> Int
focusIndexOf myWindow order model = case activeTabInWindow myWindow model of
  Just tid -> fromMaybe (-1) (Array.findIndex (\o -> o.id == tid) order)
  Nothing -> -1

viewStats :: String -> Model -> ViewStats
viewStats query model =
  { nodeTotal: Map.size model.nodes
  , openTabTotal:
      foldlWithIndex
        (\tabId total nodeId -> case Map.lookup nodeId model.nodes of
          Just node | node.kind == KTab && node.tabId == Just tabId -> total + 1
          _ -> total
        )
        0
        model.byTab
  , matchTotal:
      if normalizeSearchQuery query == "" then 0
      else
        foldl
          (\n node -> if matchesSearch query node then n + 1 else n)
          0
          (Map.values model.nodes)
  }

-- Wire codec: Kind travels as a string (reusing Codec's kind tags). ----------

type RowWire =
  { id :: NodeId
  , index :: Int
  , depth :: Int
  , subtreeEnd :: Int
  , kind :: String
  , title :: String
  , url :: Maybe String
  , favIconUrl :: Maybe String
  , live :: Boolean
  , active :: Boolean
  , collapsed :: Boolean
  , hasChildren :: Boolean
  , isLastRoot :: Boolean
  , isSearchMatch :: Maybe Boolean
  }

rowToWire :: ViewRow -> RowWire
rowToWire r =
  { id: r.id, index: r.index, depth: r.depth, subtreeEnd: r.subtreeEnd, kind: kindStr r.kind
  , title: r.title, url: r.url, favIconUrl: r.favIconUrl, live: r.live, active: r.active, collapsed: r.collapsed
  , hasChildren: r.hasChildren, isLastRoot: r.isLastRoot, isSearchMatch: if r.isSearchMatch then Just true else Nothing
  }

rowFromWire :: RowWire -> ViewRow
rowFromWire r =
  { id: r.id, index: r.index, depth: r.depth, subtreeEnd: r.subtreeEnd, kind: parseKind r.kind
  , title: r.title, url: r.url, favIconUrl: r.favIconUrl, live: r.live, active: r.active, collapsed: r.collapsed
  , hasChildren: r.hasChildren, isLastRoot: r.isLastRoot, isSearchMatch: r.isSearchMatch == Just true
  }

encodeView :: View -> Json
encodeView v = encodeJson
  { total: v.total
  , rows: map rowToWire v.rows
  , focusIndex: v.focusIndex
  , serverMs: v.serverMs
  , nodeTotal: v.nodeTotal
  , openTabTotal: v.openTabTotal
  , matchTotal: v.matchTotal
  }

decodeView :: Json -> Either String View
decodeView json = do
  rec <- lmap printJsonDecodeError
    ( decodeJson json :: Either _
        { total :: Int
        , rows :: Array RowWire
        , focusIndex :: Int
        , serverMs :: Number
        , nodeTotal :: Maybe Int
        , openTabTotal :: Maybe Int
        , matchTotal :: Maybe Int
        }
    )
  pure
    { total: rec.total
    , rows: map rowFromWire rec.rows
    , focusIndex: rec.focusIndex
    , serverMs: rec.serverMs
    , nodeTotal: fromMaybe rec.total rec.nodeTotal
    , openTabTotal: fromMaybe 0 rec.openTabTotal
    , matchTotal: fromMaybe 0 rec.matchTotal
    }
