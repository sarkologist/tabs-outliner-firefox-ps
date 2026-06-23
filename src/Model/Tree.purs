-- | Pure tree operations over the Model. None of these is O(total) except the
-- | explicitly on-demand `searchIds`; `visible` is O(visible) (it never
-- | descends collapsed subtrees), and structural edits are O(siblings/subtree).
module Model.Tree where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Model.Types (Model, Node, NodeId, Patch, Status(..), displayTitle)

-- | Apply a patch to a model: upsert nodes, delete removed ones, update roots,
-- | and keep the live indexes current. Shared by the background (authority) and
-- | the sidebar (view), which is what keeps the two consistent by construction.
-- | Stale index entries left by closed/removed nodes are harmless: a browser id
-- | is never looked up after its object is gone, and is overwritten if reused.
applyPatch :: Patch -> Model -> Model
applyPatch p model =
  let
    nodes1 = foldl (\m n -> Map.insert n.id n m) model.nodes p.upserts
    nodes2 = foldl (\m i -> Map.delete i m) nodes1 p.removes
    byTab1 = foldl indexTab model.byTab p.upserts
    byWin1 = foldl indexWin model.byWindow p.upserts
  in
    model
      { nodes = nodes2
      , byTab = byTab1
      , byWindow = byWin1
      , roots = fromMaybe model.roots p.roots
      }
  where
  indexTab m n = case n.tabId of
    Just t | isLive n -> Map.insert t n.id m
    _ -> m
  indexWin m n = case n.windowId of
    Just w | isLive n -> Map.insert w n.id m
    _ -> m

isLive :: Node -> Boolean
isLive n = case n.status of
  Live -> true
  Closed -> false

lookupNode :: NodeId -> Model -> Maybe Node
lookupNode id model = Map.lookup id model.nodes

-- | The window node id a live tab id is bound to, if any.
windowNodeOf :: Int -> Model -> Maybe NodeId
windowNodeOf w model = Map.lookup w model.byWindow

tabNodeOf :: Int -> Model -> Maybe NodeId
tabNodeOf t model = Map.lookup t model.byTab

type Entry = { id :: NodeId, depth :: Int }

-- | Visible nodes in preorder, paired with depth. Stops at collapsed nodes, so
-- | the cost is O(visible), not O(total). Built with a difference-list style
-- | accumulator to stay linear (no quadratic array concatenation).
visible :: Model -> Array Entry
visible model = Array.fromFoldable (foldr (go 0) Nil model.roots)
  where
  go :: Int -> NodeId -> List Entry -> List Entry
  go depth id rest = case Map.lookup id model.nodes of
    Nothing -> rest
    Just n ->
      let
        kids = if n.collapsed then [] else n.children
      in
        Cons { id, depth } (foldr (go (depth + 1)) rest kids)

-- | All ids in the subtree rooted at `root` (including `root`), preorder.
-- | O(subtree).
subtreeIds :: NodeId -> Model -> Array NodeId
subtreeIds root model = Array.fromFoldable (go root Nil)
  where
  go :: NodeId -> List NodeId -> List NodeId
  go id rest = case Map.lookup id model.nodes of
    Nothing -> rest
    Just n -> Cons id (foldr go rest n.children)

-- | Case-insensitive substring search over display title and url. O(total),
-- | but only ever run on demand (user typed a query).
searchIds :: String -> Model -> Array NodeId
searchIds query model =
  let
    q = String.toLower query
    match n =
      String.contains (String.Pattern q) (String.toLower (displayTitle n))
        || maybe' false (\u -> String.contains (String.Pattern q) (String.toLower u)) n.url
  in
    Array.mapMaybe (\n -> if match n then Just n.id else Nothing)
      (Array.fromFoldable (Map.values model.nodes))
  where
  maybe' d f = case _ of
    Nothing -> d
    Just x -> f x

-- Array helpers --------------------------------------------------------------

insertAtClamped :: forall a. Int -> a -> Array a -> Array a
insertAtClamped i x xs =
  let
    n = Array.length xs
    i' = clamp 0 n i
  in
    fromMaybe (Array.snoc xs x) (Array.insertAt i' x xs)

-- | Move `x` to `toIdx` among its siblings (array-index, clamped). Exact when
-- | no closed nodes are interleaved; approximate otherwise (a waived edge case).
moveWithin :: forall a. Eq a => a -> Int -> Array a -> Array a
moveWithin x toIdx xs = insertAtClamped toIdx x (Array.delete x xs)
