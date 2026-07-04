-- | Pure tree operations over the Model. None of these is O(total) except the
-- | explicitly on-demand `searchIds`; `visible` is O(visible) (it never
-- | descends collapsed subtrees), and structural edits are O(siblings/subtree).
module Model.Tree where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Set (Set)
import Data.Set as Set
import Model.Search (matchesSearch, normalizeSearchQuery)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, emptyPatch, isLive, isLiveTab)

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

-- | The STRUCTURAL test for "this container is a live window": it owns at least
-- | one live tab somewhere in its subtree. At runtime the operative marker is the
-- | container's `windowId` binding (O(1), and what display and the indexes key
-- | off); the two are kept loosely in step — e.g. a freshly-opened window is
-- | windowId-bound for a moment before its first tab node lands.
isLiveWindow :: Model -> Node -> Boolean
isLiveWindow model n = n.kind == KGroup && liveTabCount model n.id > 0

-- | Is child id `cid` a LIVE TAB in `model`? This is the membership that defines
-- | browser tab order among a container's children: a window's live-tab children,
-- | filtered in array order, are required to match the browser's tab order (see
-- | `insertAtLive`/`liveInsertIndex`). Closed/history tab nodes and sub-groups are
-- | not live tabs.
liveTabChild :: Model -> NodeId -> Boolean
liveTabChild model cid = maybe false isLiveTab (Map.lookup cid model.nodes)

-- | Nearest ancestor that is a group/container, skipping tab ancestors. This is
-- | the runtime window boundary for nested tab trees: a tab can own semantic
-- | children, but only a group can bind/create a browser window.
nearestGroupAncestor :: Model -> NodeId -> Maybe Node
nearestGroupAncestor model nid = go Set.empty (Map.lookup nid model.nodes >>= _.parent)
  where
  go seen = case _ of
    Nothing -> Nothing
    Just pid
      | Set.member pid seen -> Nothing
      | otherwise -> case Map.lookup pid model.nodes of
          Just n | n.kind == KGroup -> Just n
          Just n -> go (Set.insert pid seen) n.parent
          Nothing -> Nothing

-- | Live tab nodes in preorder under `root`. This is the nodes-side ordering that
-- | corresponds to a browser window's tab strip when tabs are nested under tabs.
-- | Descendant groups that are themselves live browser windows are separate
-- | runtime boundaries, so they are not counted in the ancestor window's order.
liveTabPreorder :: Model -> NodeId -> Array NodeId
liveTabPreorder model root = Array.fromFoldable (go root Nil)
  where
  go :: NodeId -> List NodeId -> List NodeId
  go id rest = case Map.lookup id model.nodes of
    Nothing -> rest
    Just n | id /= root && n.kind == KGroup && n.windowId /= Nothing -> rest
    Just n ->
      let tail = foldr go rest n.children
      in if n.kind == KTab && isLiveTab n then Cons id tail else tail

liveTabCount :: Model -> NodeId -> Int
liveTabCount model root = liveTabCountInWindow model root root

-- | Count live tabs in `root` as seen from `windowRoot`'s runtime tab strip.
-- | A descendant live group belongs to its own browser window, so it contributes
-- | zero tabs to the ancestor window.
liveTabCountInWindow :: Model -> NodeId -> NodeId -> Int
liveTabCountInWindow model windowRoot root = go root
  where
  go id = case Map.lookup id model.nodes of
    Nothing -> 0
    Just n | id /= windowRoot && n.kind == KGroup && n.windowId /= Nothing -> 0
    Just n ->
      let self = if n.kind == KTab && isLiveTab n then 1 else 0
      in self + foldl (\count cid -> count + go cid) 0 n.children

liveTabPreorderIndex :: Model -> NodeId -> NodeId -> Maybe Int
liveTabPreorderIndex model root id = Array.elemIndex id (liveTabPreorder model root)

type LiveInsertSlot = { parent :: NodeId, index :: Int }

-- | Find a tree insertion slot for a live tab that should appear at `liveIdx` in
-- | the owning window's live-tab preorder. A preferred parent (usually the
-- | opener tab) wins when it can represent the requested order exactly; otherwise
-- | direct window placement is preferred, then any exact preorder slot.
liveInsertSlot :: Model -> NodeId -> Maybe NodeId -> Int -> LiveInsertSlot
liveInsertSlot model windowRoot preferredParent liveIdx =
  let
    target = clamp 0 (liveTabCount model windowRoot) liveIdx
    direct = exactSlotInParent windowRoot target
    anySlot = firstExactSlot windowRoot target
    append =
      { parent: windowRoot
      , index: maybe 0 (Array.length <<< _.children) (Map.lookup windowRoot model.nodes)
      }
  in
    fromMaybe
      (fromMaybe (fromMaybe append anySlot) direct)
      (preferredParent >>= \pid -> exactSlotInParent pid target)
  where
  exactSlotInParent :: NodeId -> Int -> Maybe LiveInsertSlot
  exactSlotInParent parentId target = do
    if inRuntimeWindow parentId then do
      p <- Map.lookup parentId model.nodes
      slot <- foldl
        (\found i -> if slotLiveIndex windowRoot parentId i == Just target then Just i else found)
        Nothing
        (Array.range 0 (Array.length p.children))
      pure { parent: parentId, index: slot }
    else Nothing

  firstExactSlot :: NodeId -> Int -> Maybe LiveInsertSlot
  firstExactSlot root target = go root
    where
    go id = case Map.lookup id model.nodes of
      Just n | isNestedLiveGroup id n -> Nothing
      Just n -> case exactSlotInParent id target of
        Just slot -> Just slot
        Nothing -> goChildren n.children
      Nothing -> Nothing

    goChildren kids = case Array.uncons kids of
      Nothing -> Nothing
      Just { head, tail } -> case go head of
        Just slot -> Just slot
        Nothing -> goChildren tail

  inRuntimeWindow :: NodeId -> Boolean
  inRuntimeWindow id = go Set.empty id
    where
    go seen cur
      | cur == windowRoot = true
      | Set.member cur seen = false
      | otherwise = case Map.lookup cur model.nodes of
          Just n | isNestedLiveGroup cur n -> false
          Just n -> maybe false (go (Set.insert cur seen)) n.parent
          Nothing -> false

  isNestedLiveGroup :: NodeId -> Node -> Boolean
  isNestedLiveGroup id n = id /= windowRoot && n.kind == KGroup && n.windowId /= Nothing

  slotLiveIndex :: NodeId -> NodeId -> Int -> Maybe Int
  slotLiveIndex root parentId slot = go 0 root
    where
    go acc id = case Map.lookup id model.nodes of
      Nothing -> Nothing
      Just n | isNestedLiveGroup id n -> Nothing
      Just n ->
        let afterSelf = if n.kind == KTab && isLiveTab n then acc + 1 else acc
        in
          if id == parentId then Just (afterSelf + liveBefore slot n.children)
          else goChildren afterSelf n.children

    goChildren acc kids = case Array.uncons kids of
      Nothing -> Nothing
      Just { head, tail } -> case go acc head of
        Just idx -> Just idx
        Nothing -> goChildren (acc + liveTabCountInWindow model windowRoot head) tail

  liveBefore slot children =
    foldl (\n cid -> n + liveTabCountInWindow model windowRoot cid)
      0
      (Array.take (clamp 0 (Array.length children) slot) children)

-- | Combine two patches applied in sequence (`b` after `a`): later upserts win
-- | (folded last), removes accumulate, and `b`'s roots — if it set them — win.
mergePatch :: Patch -> Patch -> Patch
mergePatch a b =
  { upserts: a.upserts <> b.upserts
  , removes: a.removes <> b.removes
  , roots: case b.roots of
      Just _ -> b.roots
      Nothing -> a.roots
  }

-- | Prune `nid` if it is now a childless, un-renamed group, then walk up doing the
-- | same to its parent. Emptying a container (by deleting or moving away its last
-- | child) thus removes it and any ancestors it leaves empty — but a renamed group
-- | is a deliberate label and is kept. Cost is O(pruned-chain depth, plus each
-- | pruned node's parent/root sibling list) — never O(total). Returns the new model
-- | and the patch of what it pruned, to fold (via `mergePatch`) into the triggering
-- | edit's own patch.
pruneFrom :: NodeId -> Model -> { model :: Model, patch :: Patch }
pruneFrom nid model = case Map.lookup nid model.nodes of
  Just n | n.kind == KGroup && Array.null n.children && isNothing n.customTitle ->
    let
      parentUpsert = case n.parent >>= (\pid -> Map.lookup pid model.nodes) of
        Just p -> [ p { children = Array.delete nid p.children } ]
        Nothing -> []
      rootsM = if Array.elem nid model.roots then Just (Array.delete nid model.roots) else Nothing
      step = { upserts: parentUpsert, removes: [ nid ], roots: rootsM }
      model' = applyPatch step model
    in
      case n.parent of
        Just pid -> let up = pruneFrom pid model' in { model: up.model, patch: mergePatch step up.patch }
        Nothing -> { model: model', patch: step }
  _ -> { model, patch: emptyPatch }

lookupNode :: NodeId -> Model -> Maybe Node
lookupNode id model = Map.lookup id model.nodes

-- | The LIVE tab node currently bound to browser tab id `t`, validating the
-- | index hit against the node's actual binding. This is what lets `applyPatch`
-- | leave stale `byTab`/`byWindow` entries in place (they are simply ignored):
-- | a reused browser id will not resurrect a closed/rebound node.
liveTabNode :: Int -> Model -> Maybe Node
liveTabNode t model = do
  nid <- Map.lookup t model.byTab
  n <- Map.lookup nid model.nodes
  if n.tabId == Just t && isLive n then Just n else Nothing

liveWindowNode :: Int -> Model -> Maybe Node
liveWindowNode w model = do
  nid <- Map.lookup w model.byWindow
  n <- Map.lookup nid model.nodes
  if n.windowId == Just w && isLive n then Just n else Nothing

-- | The topmost ancestor of `nid` (the root of the tree it sits in) — walks
-- | parent links to the node that has no parent. O(depth). A node with no parent
-- | is its own root ancestor. Used to position a "move to top level" just after
-- | the root the node currently belongs to. The visited set is a corruption
-- | backstop: a parent cycle (only reachable via tampered persisted data — every
-- | command keeps the forest acyclic) stops the walk instead of looping forever.
rootAncestor :: NodeId -> Model -> NodeId
rootAncestor = go Set.empty
  where
  go seen nid model
    | Set.member nid seen = nid
    | otherwise = case Map.lookup nid model.nodes >>= _.parent of
        Just pid -> go (Set.insert nid seen) pid model
        Nothing -> nid

-- | Is `ancestor` an ancestor of (or equal to) `start`? Walks parent links
-- | upward — O(depth), not O(subtree). Used for move cycle-detection.
isAncestorOrSelf :: NodeId -> NodeId -> Model -> Boolean
isAncestorOrSelf ancestor start model = go (Just start)
  where
  go Nothing = false
  go (Just cur)
    | cur == ancestor = true
    | otherwise = go (Map.lookup cur model.nodes >>= _.parent)

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

-- | Rows to show for a query: every match plus its ancestors (so the path is
-- | visible), in preorder, ignoring collapse — matches inside collapsed groups
-- | still appear. O(total), on demand only.
searchVisible :: String -> Model -> Array Entry
searchVisible query model = Array.fromFoldable (foldr (go 0) Nil model.roots)
  where
  shown = ancestorClosure (searchIds query model) model
  go :: Int -> NodeId -> List Entry -> List Entry
  go depth id rest
    | Set.member id shown = case Map.lookup id model.nodes of
        Just n -> Cons { id, depth } (foldr (go (depth + 1)) rest n.children)
        Nothing -> rest
    | otherwise = rest

-- | A set containing every given id and all of its ancestors.
ancestorClosure :: Array NodeId -> Model -> Set NodeId
ancestorClosure ids model = foldl (\s id -> goUp s (Just id)) Set.empty ids
  where
  goUp s = case _ of
    Nothing -> s
    Just cur
      | Set.member cur s -> s -- already added this node and its ancestors
      | otherwise -> goUp (Set.insert cur s) (Map.lookup cur model.nodes >>= _.parent)

-- | Case-insensitive substring search over display title and url. O(total),
-- | but only ever run on demand (user typed a query).
searchIds :: String -> Model -> Array NodeId
searchIds query model =
  let
    q = normalizeSearchQuery query
  in
    Array.mapMaybe (\n -> if matchesSearch q n then Just n.id else Nothing)
      (Array.fromFoldable (Map.values model.nodes))

-- Array helpers --------------------------------------------------------------

insertAtClamped :: forall a. Int -> a -> Array a -> Array a
insertAtClamped i x xs =
  let
    n = Array.length xs
    i' = clamp 0 n i
  in
    fromMaybe (Array.snoc xs x) (Array.insertAt i' x xs)

-- | The array position at which to insert so that the new element becomes the
-- | `liveIdx`-th element satisfying `p`. Elements that fail `p` — the closed /
-- | history nodes interleaved among a window's live tabs — keep their relative
-- | order: the new element lands immediately before the live element currently at
-- | `liveIdx`, or at the very end when `liveIdx` is at/after the live count (a
-- | negative `liveIdx` clamps to before the first live element). This is what maps
-- | a browser tab `index` — a position counted among LIVE tabs only — to a
-- | children-array index, so inserting or moving a live tab keeps the invariant
-- | `filter p children == browser tab order`. O(scanned siblings).
liveInsertIndex :: forall a. (a -> Boolean) -> Int -> Array a -> Int
liveInsertIndex p liveIdx xs = go 0 0
  where
  n = Array.length xs
  go i live
    | i >= n = n -- ran out of live elements: append (covers liveIdx >= live count)
    | otherwise = case Array.index xs i of
        Just x | p x -> if live >= liveIdx then i else go (i + 1) (live + 1)
        _ -> go (i + 1) live -- a non-live (closed) node: keep it before the new element

-- | Insert `x` so it becomes the `liveIdx`-th element satisfying `p`, mapping a
-- | browser (live-only) index to the array position via `liveInsertIndex`.
insertAtLive :: forall a. (a -> Boolean) -> Int -> a -> Array a -> Array a
insertAtLive p liveIdx x xs = insertAtClamped (liveInsertIndex p liveIdx xs) x xs

-- | Move `x` to live-index `toIdx` among the siblings satisfying `p`: `x` is
-- | removed, then re-inserted just before the live element now at `toIdx` (or at
-- | the end). Exact even when closed/history nodes are interleaved — the live
-- | subsequence ends up in browser order. (`p` need not exclude `x` itself; it is
-- | removed first regardless.)
moveWithin :: forall a. Eq a => (a -> Boolean) -> a -> Int -> Array a -> Array a
moveWithin p x toIdx xs =
  let xs' = Array.delete x xs
  in insertAtClamped (liveInsertIndex p toIdx xs') x xs'
