-- | The projection: how the background turns its model into the windowed `View`
-- | the sidebar renders. The sidebar never holds the model — it asks for a slice
-- | of the visible order and gets back exactly the rows in view. Pure (so it is
-- | unit-tested without a browser); the background owns the order cache around it.
module Model.View
  ( ViewRow
  , View
  , OrderEntry
  , computeOrder
  , sliceView
  , focusIndexOf
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
import Data.Maybe (Maybe(..), fromMaybe)
import Model.Codec (kindStr, parseKind)
import Model.Scroll (activeTabInWindow)
import Model.Tree (Entry, searchVisible, visible)
import Model.Types (Kind, Model, NodeId, displayTitle, isLive)

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
  , live :: Boolean
  , active :: Boolean
  , collapsed :: Boolean
  , hasChildren :: Boolean
  , isLastRoot :: Boolean
  }

-- `serverMs` is the background's own compute time for this window (0 unless
-- profiling), so the sidebar can split the round-trip into compute vs transport.
type View = { total :: Int, rows :: Array ViewRow, focusIndex :: Int, serverMs :: Number }

-- | The full visible order for a query, each entry tagged with its subtree end.
-- | `visible`/`searchVisible` are reused as-is; `withSubtreeEnds` is one O(N) pass.
computeOrder :: String -> Model -> Array OrderEntry
computeOrder query model =
  withSubtreeEnds (if query == "" then visible model else searchVisible query model)

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
sliceView :: Model -> Array OrderEntry -> Int -> Int -> Array ViewRow
sliceView model order start count =
  let
    total = Array.length order
    -- clamp to the last legal window, so a start past a shrunk order (collapse/
    -- delete while scrolled near the bottom) shows the tail, not a blank window
    s = clamp 0 (max 0 (total - count)) start
    lastRoot = Array.last model.roots
    window = Array.slice s (s + count) order
  in
    Array.catMaybes (Array.mapWithIndex (\off oe -> toRow model lastRoot (s + off) oe) window)

toRow :: Model -> Maybe NodeId -> Int -> OrderEntry -> Maybe ViewRow
toRow model lastRoot i oe = Map.lookup oe.id model.nodes <#> \n ->
  { id: n.id
  , index: i
  , depth: oe.depth
  , subtreeEnd: oe.subtreeEnd
  , kind: n.kind
  , title: displayTitle n
  , live: isLive n
  , active: n.active
  , collapsed: n.collapsed
  , hasChildren: not (Array.null n.children)
  , isLastRoot: Just n.id == lastRoot
  }

-- | Flat index of this window's active tab in the order, or -1. O(order), so the
-- | background only computes it when the sidebar asks (open / focus change).
focusIndexOf :: Int -> Array OrderEntry -> Model -> Int
focusIndexOf myWindow order model = case activeTabInWindow myWindow model of
  Just tid -> fromMaybe (-1) (Array.findIndex (\o -> o.id == tid) order)
  Nothing -> -1

-- Wire codec: Kind travels as a string (reusing Codec's kind tags). ----------

type RowWire =
  { id :: NodeId
  , index :: Int
  , depth :: Int
  , subtreeEnd :: Int
  , kind :: String
  , title :: String
  , live :: Boolean
  , active :: Boolean
  , collapsed :: Boolean
  , hasChildren :: Boolean
  , isLastRoot :: Boolean
  }

rowToWire :: ViewRow -> RowWire
rowToWire r =
  { id: r.id, index: r.index, depth: r.depth, subtreeEnd: r.subtreeEnd, kind: kindStr r.kind
  , title: r.title, live: r.live, active: r.active, collapsed: r.collapsed
  , hasChildren: r.hasChildren, isLastRoot: r.isLastRoot
  }

rowFromWire :: RowWire -> ViewRow
rowFromWire r =
  { id: r.id, index: r.index, depth: r.depth, subtreeEnd: r.subtreeEnd, kind: parseKind r.kind
  , title: r.title, live: r.live, active: r.active, collapsed: r.collapsed
  , hasChildren: r.hasChildren, isLastRoot: r.isLastRoot
  }

encodeView :: View -> Json
encodeView v = encodeJson { total: v.total, rows: map rowToWire v.rows, focusIndex: v.focusIndex, serverMs: v.serverMs }

decodeView :: Json -> Either String View
decodeView json = do
  rec <- lmap printJsonDecodeError
    (decodeJson json :: Either _ { total :: Int, rows :: Array RowWire, focusIndex :: Int, serverMs :: Number })
  pure { total: rec.total, rows: map rowFromWire rec.rows, focusIndex: rec.focusIndex, serverMs: rec.serverMs }
