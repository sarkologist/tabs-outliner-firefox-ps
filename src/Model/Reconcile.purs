-- | The browser-event half of the reducer: deterministic, pure, O(change) per
-- | event. This single function replaces the original's reconciliation engine
-- | (confidence-merge, snapshot corroboration, runtime-trace hunting). Edge
-- | cases around interleaved closed nodes / simultaneous cross-window moves are
-- | deliberately approximated (waived), not bulletproofed.
module Model.Reconcile where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Model.Event (BrowserEvent(..), OpenedTab)
import Model.Tree (applyPatch, insertAtClamped, liveTabNode, liveWindowNode, mergePatch, moveWithin, pruneFrom, subtreeIds)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, Step, defaultNode, emptyPatch, isLive)

mkId :: Int -> NodeId
mkId i = "n" <> show i

noop :: Model -> Step
noop model = { model, patch: emptyPatch }

-- | Apply a patch to `model` and stamp the new id counter.
commit :: Int -> Patch -> Model -> Step
commit nextId patch model = { model: (applyPatch patch model) { nextId = nextId }, patch }

withTab :: Int -> Model -> (NodeId -> Node -> Step) -> Step
withTab tabId model f = case liveTabNode tabId model of
  Nothing -> noop model
  Just n -> f n.id n

-- | The window node id for a browser window, reusing an existing node or
-- | producing a fresh (not-yet-inserted) one. Callers fold `winNode` into their
-- | own patch, so there is exactly one applyPatch per event.
resolveWindow
  :: Number
  -> Int
  -> Model
  -> { winId :: NodeId, winNode :: Node, isNew :: Boolean, nextId :: Int }
resolveWindow now windowId model = case liveWindowNode windowId model of
  Just n -> { winId: n.id, winNode: n, isNew: false, nextId: model.nextId }
  Nothing ->
    let
      nid = mkId model.nextId
    in
      { winId: nid
      , winNode: (defaultNode nid KGroup now) { windowId = Just windowId, title = "Window" }
      , isNew: true
      , nextId: model.nextId + 1
      }

applyBrowser :: Number -> BrowserEvent -> Model -> Step
applyBrowser now ev model = case ev of
  WindowOpened { windowId } -> case Map.lookup windowId model.byWindow of
    Just _ -> noop model
    Nothing -> case Array.uncons model.pendingRestoreWindows of
      -- a window restore is pending: bind this new browser window to the closed
      -- window node being restored, so it goes live in place (its tabs rebind as
      -- their own onCreated events arrive) rather than a fresh node.
      Just { head: wNid, tail } ->
        let model' = model { pendingRestoreWindows = tail }
        in case Map.lookup wNid model'.nodes of
          Just wn | wn.kind == KGroup -> bindRestoredWindow wNid windowId wn model'
          -- the restored node was deleted before its window opened: drop the
          -- stale queue entry and treat this as a brand-new window.
          _ -> freshWindow now windowId model'
      Nothing -> freshWindow now windowId model

  WindowClosed { windowId } -> case Map.lookup windowId model.byWindow of
    Nothing -> noop model
    Just wid ->
      let
        upserts = Array.mapMaybe (\i -> closeNode now <$> Map.lookup i model.nodes)
          (subtreeIds wid model)
        patch = { upserts, removes: [], roots: Nothing }
      in
        commit model.nextId patch model

  TabOpened t -> openTab now t model

  TabClosed { tabId } -> withTab tabId model \_ n ->
    commit model.nextId { upserts: [ closeNode now n ], removes: [], roots: Nothing } model

  TabChanged c -> withTab c.tabId model \_ n ->
    let
      n' = n
        { title = fromMaybe n.title c.title
        , url = orElse n.url c.url
        , favIconUrl = orElse n.favIconUrl c.favIconUrl
        }
    in
      commit model.nextId { upserts: [ n' ], removes: [], roots: Nothing } model

  TabActivated a -> activateTab a.tabId a.windowId model

  TabMoved m -> withTab m.tabId model \nid n -> case n.parent of
    Nothing -> noop model
    Just pid -> case Map.lookup pid model.nodes of
      Nothing -> noop model
      Just p ->
        let
          p' = p { children = moveWithin nid m.toIndex p.children }
        in
          commit model.nextId { upserts: [ p' ], removes: [], roots: Nothing } model

  TabAttached a -> attachTab now a.tabId a.windowId a.index model

-- | A brand-new browser window: add a fresh window node at the end of the roots.
freshWindow :: Number -> Int -> Model -> Step
freshWindow now windowId model =
  let
    rw = resolveWindow now windowId model
    patch = { upserts: [ rw.winNode ], removes: [], roots: Just (model.roots <> [ rw.winId ]) }
  in
    commit rw.nextId patch model

-- | Flip one live node to closed history by dropping its browser binding: a live
-- | tab loses its `tabId`, a live-window container loses its `windowId` (becoming
-- | a plain saved group). Nodes with no binding (plain sub-groups, already-closed
-- | tabs) are left untouched — closing a window thus turns it into a saved group
-- | holding closed tabs, with nested user groups preserved.
closeNode :: Number -> Node -> Node
closeNode now n
  | isLive n = n { tabId = Nothing, windowId = Nothing, active = false, closedAt = Just now }
  | otherwise = n

orElse :: Maybe String -> Maybe String -> Maybe String
orElse old new = case new of
  Just _ -> new
  Nothing -> old

openTab :: Number -> OpenedTab -> Model -> Step
openTab now t model = case liveTabNode t.tabId model of
  Just _ -> noop model -- already tracking this browser tab; ignore duplicate
  Nothing -> case bindPendingWindowForTab t.windowId model of
    -- tabs-before-window: this tab is the first sign of a restored window whose
    -- WindowOpened hasn't arrived (Firefox does not order windows.onCreated before
    -- the new window's tabs.onCreated). Bind the queued window node onto this new
    -- browser window, then rebind this tab from the queue that bind just populated —
    -- instead of minting a duplicate window + tab and stranding the originals.
    Just bound -> andThen bound (rebindOrFresh now t bound.model)
    -- window already known (WindowOpened arrived first), or no restore is pending
    Nothing -> rebindOrFresh now t model

-- | Pop the node queued to rebind in this tab's window and re-use it; open fresh if
-- | the queue is empty or the queued node vanished. The window + creation-order
-- | match is deliberate — the browser may report a different url for the recreated
-- | tab than the one stored.
rebindOrFresh :: Number -> OpenedTab -> Model -> Step
rebindOrFresh now t model = case popPendingRestore t.windowId model of
  Just r -> case Map.lookup r.node r.model.nodes of
    Just n -> rebindRestored now t r.node n r.model
    Nothing -> openFresh now t r.model -- queued node vanished; consume the slot, open fresh
  Nothing -> openFresh now t model

-- | Sequence two steps: keep the later model, combine both patches (for persist +
-- | broadcast).
andThen :: Step -> Step -> Step
andThen a b = { model: b.model, patch: mergePatch a.patch b.patch }

-- | The tabs-before-window restore binding. Firefox does not guarantee a new
-- | window's `windows.onCreated` precedes its tabs' `tabs.onCreated`, so a restored
-- | window's first tab can surface before its `WindowOpened`. When it does — an
-- | unknown window, a restore pending — bind the head of the pending-restore-window
-- | queue onto this new browser window, exactly what `WindowOpened` does when it
-- | arrives first; the caller then rebinds the tab. A stale head (its node deleted
-- | meanwhile) is dropped, also like `WindowOpened`, so it can't wedge later
-- | restores. `Nothing` when the window is already known or no restore is pending.
-- |
-- | Trade-off (accepted, in keeping with this module's waived interleavings): as
-- | with the existing `WindowOpened` FIFO, a brand-new *user* window opening during
-- | the brief CreateWindow round-trip can be mistaken for the restored one — now via
-- | its first tab too, not only its window event.
bindPendingWindowForTab :: Int -> Model -> Maybe Step
bindPendingWindowForTab windowId model
  | isJust (liveWindowNode windowId model) = Nothing
  | otherwise = case Array.uncons model.pendingRestoreWindows of
      Nothing -> Nothing
      Just { head: wNid, tail } ->
        let model' = model { pendingRestoreWindows = tail }
        in case Map.lookup wNid model'.nodes of
          Just wn | wn.kind == KGroup -> Just (bindRestoredWindow wNid windowId wn model')
          -- stale head: drop it (the queue is advanced in model') and let the caller
          -- open this tab fresh, so a deleted restore can't block the next one
          _ -> Just (noop model')

-- | Bind closed window node `wNid` onto freshly-opened browser window `windowId`:
-- | it goes live in place and its restorable direct-child tabs are queued (FIFO) so
-- | each rebinds in creation order as its onCreated arrives, instead of a duplicate
-- | window/tab being spawned. Shared by both arrival orders (`WindowOpened`, or the
-- | window's first tab when its WindowOpened lags). `model` must already have the
-- | queue head advanced.
bindRestoredWindow :: NodeId -> Int -> Node -> Model -> Step
bindRestoredWindow wNid windowId wn model =
  let
    wn' = wn { windowId = Just windowId, closedAt = Nothing }
    model' = model { pendingRestore = Map.insert windowId (restorableTabs wNid model) model.pendingRestore }
    patch = { upserts: [ wn' ], removes: [], roots: Nothing }
  in
    commit model'.nextId patch model'

-- | Pop the next node queued to rebind in `windowId` (FIFO), returning it and the
-- | model with the queue advanced.
popPendingRestore :: Int -> Model -> Maybe { node :: NodeId, model :: Model }
popPendingRestore windowId model = do
  lst <- Map.lookup windowId model.pendingRestore
  { head, tail } <- List.uncons lst
  let
    pr =
      if List.null tail then Map.delete windowId model.pendingRestore
      else Map.insert windowId tail model.pendingRestore
  pure { node: head, model: model { pendingRestore = pr } }

-- | The window's restorable tabs, in the order their recreated tabs' onCreated will
-- | arrive: every closed-with-url tab in its tab FOREST — direct children and tabs
-- | nested under those tabs (the original outline nests tabs under tabs) — in
-- | preorder, NOT descending into a nested sub-window (a child group), whose own
-- | tabs open in their own window. This lines the rebind FIFO up with the preorder
-- | `Command.restore` lays the CreateWindow urls out in.
restorableTabs :: NodeId -> Model -> List NodeId
restorableTabs root model = List.fromFoldable (go root)
  where
  go nid = case Map.lookup nid model.nodes of
    Nothing -> []
    Just n ->
      let
        here = if n.kind == KTab && isNothing n.tabId && isJust n.url then [ nid ] else []
        -- descend through the window itself and through tabs (tabs nest), but halt
        -- at a nested group — that is a separate window with its own restore
        kids = if nid == root || n.kind == KTab then Array.concatMap go n.children else []
      in
        here <> kids

-- | A brand-new tab: create a node under its (possibly new) window.
openFresh :: Number -> OpenedTab -> Model -> Step
openFresh now t model =
  let
    rw = resolveWindow now t.windowId model
    tabNodeId = mkId rw.nextId
    tabNode = (defaultNode tabNodeId KTab now)
      { title = t.title
      , url = t.url
      , favIconUrl = t.favIconUrl
      , active = t.active
      , tabId = Just t.tabId
      , parent = Just rw.winId
      }
    winNode' = rw.winNode { children = insertAtClamped t.index tabNodeId rw.winNode.children }
    roots' = if rw.isNew then Just (model.roots <> [ rw.winId ]) else Nothing
    patch = { upserts: [ winNode', tabNode ], removes: [], roots: roots' }
  in
    commit (rw.nextId + 1) patch model

-- | A restored closed tab re-using its existing node (no duplicate). The node goes
-- | Live again; the pending-restore queue was already advanced by `popPendingRestore`.
-- | If it was nested under ANOTHER tab — the original outline allows that, a browser
-- | window cannot — it is flattened to a direct child of its window at the browser
-- | position, keeping the model's invariant that a live tab's parent is its window.
-- | A tab already directly under its window (the common case) stays exactly in place.
rebindRestored :: Number -> OpenedTab -> NodeId -> Node -> Model -> Step
rebindRestored _ t nid n model =
  let
    live = n
      { tabId = Just t.tabId
      , active = t.active
      , title = t.title
      , url = t.url
      , favIconUrl = t.favIconUrl
      , closedAt = Nothing
      }
  in
    case liveWindowNode t.windowId model of
      Just w | n.parent /= Just w.id ->
        let
          oldParentUpsert = case n.parent >>= (\pid -> Map.lookup pid model.nodes) of
            Just p -> [ p { children = Array.delete nid p.children } ]
            Nothing -> []
          w' = w { children = insertAtClamped t.index nid (Array.delete nid w.children) }
          patch = { upserts: oldParentUpsert <> [ w', live { parent = Just w.id } ], removes: [], roots: Nothing }
        in
          commit model.nextId patch model
      -- already a direct child of its window, or window not yet known: rebind in place
      _ -> commit model.nextId { upserts: [ live ], removes: [], roots: Nothing } model

activateTab :: Int -> Int -> Model -> Step
activateTab tabId windowId model = case liveTabNode tabId model of
  Nothing -> noop model
  Just n ->
    let
      winChildren = case liveWindowNode windowId model of
        Just w -> w.children
        Nothing -> []
      deact = Array.mapMaybe deactivate winChildren
      deactivate cid = case Map.lookup cid model.nodes of
        Just c | c.active, c.id /= n.id -> Just (c { active = false })
        _ -> Nothing
      patch = { upserts: deact <> [ n { active = true } ], removes: [], roots: Nothing }
    in
      commit model.nextId patch model

attachTab :: Number -> Int -> Int -> Int -> Model -> Step
attachTab now tabId windowId index model = withTab tabId model \nid n ->
  let
    rw = resolveWindow now windowId model
    oldParentUpsert = case n.parent of
      Just pid | pid /= rw.winId -> case Map.lookup pid model.nodes of
        Just p -> [ p { children = Array.delete nid p.children } ]
        Nothing -> []
      _ -> []
    winNode' = rw.winNode { children = insertAtClamped index nid (Array.delete nid rw.winNode.children) }
    n' = n { parent = Just rw.winId }
    roots' = if rw.isNew then Just (model.roots <> [ rw.winId ]) else Nothing
    patch = { upserts: oldParentUpsert <> [ winNode', n' ], removes: [], roots: roots' }
    base = commit rw.nextId patch model
  in
    -- the tab left its old parent; if that emptied an un-renamed group, prune it
    case n.parent of
      Just pid | pid /= rw.winId ->
        let p = pruneFrom pid base.model in base { model = p.model, patch = mergePatch base.patch p.patch }
      _ -> base
