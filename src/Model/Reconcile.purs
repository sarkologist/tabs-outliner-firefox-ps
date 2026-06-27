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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
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
      Just { head: pw, tail } ->
        let model' = model { pendingRestoreWindows = tail }
        in case Map.lookup pw.node model'.nodes of
          Just wn | wn.kind == KGroup ->
            let
              wn' = wn { windowId = Just windowId, closedAt = Nothing }
              -- queue EXACTLY the tabs this restore opens into the window (carried on
              -- the pending entry, in creation order), so each rebinds as its
              -- onCreated arrives. A rehome carries none (its dragged tab arrives via
              -- onAttached); a partial restore carries only the chosen tab(s) — so
              -- neither hijacks the container's other saved closed tabs.
              model'' =
                if List.null pw.tabs then model'
                else model' { pendingRestore = Map.insert windowId pw.tabs model'.pendingRestore }
              patch = { upserts: [ wn' ], removes: [], roots: Nothing }
            in
              commit model''.nextId patch model''
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

  TabClosed { tabId } ->
    let
      -- did the outliner ask for this close (a CloseNode "save & close"), or did
      -- the browser? Consume the marker up front, so it never leaks even if the
      -- node is already gone.
      outlinerInitiated = Set.member tabId model.closingTabs
      model' = model { closingTabs = Set.delete tabId model.closingTabs }
    in
      withTab tabId model' \nid n ->
        if n.restoredFromClosed && not outlinerInitiated
        -- a browser-initiated close of a tab the user had restored from history:
        -- the restored copy has served its purpose, so drop the node rather than
        -- re-saving it as closed history (the whole point of restoring was to use
        -- it, not to re-accumulate it). An outliner-initiated close still keeps it.
        then dropNode nid n model'
        else commit model'.nextId { upserts: [ closeNode now n ], removes: [], roots: Nothing } model'

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
-- | holding closed tabs, with nested user groups preserved. Going closed also
-- | resets `restoredFromClosed`: the live binding is gone, so its restore origin
-- | no longer applies (a fresh restore will set it again).
closeNode :: Number -> Node -> Node
closeNode now n
  | isLive n = n { tabId = Nothing, windowId = Nothing, active = false, closedAt = Just now, restoredFromClosed = false }
  | otherwise = n

-- | Remove a single browser-closed tab node from the tree entirely (used only when
-- | a *restored* tab is closed by the browser): unlink it from its parent and the
-- | node map, drop it from the roots if it sat there, then prune a parent left
-- | empty. This is `Command.Delete`'s structural removal for the one closing tab —
-- | a tab carries no children, so the subtree is just the node itself.
dropNode :: NodeId -> Node -> Model -> Step
dropNode nid n model =
  let
    parentUpsert = case n.parent >>= (\pid -> Map.lookup pid model.nodes) of
      Just p -> [ p { children = Array.delete nid p.children } ]
      Nothing -> []
    rootsM = if Array.elem nid model.roots then Just (Array.delete nid model.roots) else Nothing
    patch = { upserts: parentUpsert, removes: subtreeIds nid model, roots: rootsM }
    base = commit model.nextId patch model
  in
    case n.parent of
      Just pid -> let p = pruneFrom pid base.model in base { model = p.model, patch = mergePatch base.patch p.patch }
      Nothing -> base

orElse :: Maybe String -> Maybe String -> Maybe String
orElse old new = case new of
  Just _ -> new
  Nothing -> old

openTab :: Number -> OpenedTab -> Model -> Step
openTab now t model = case liveTabNode t.tabId model of
  Just _ -> noop model -- already tracking this browser tab; ignore duplicate
  Nothing -> case popPendingRestore t.windowId model of
    -- a restore into this window is pending: rebind the next queued node to this
    -- tab (matched by window + creation order, NOT url, which the browser may
    -- report differently for the recreated tab)
    Just r -> case Map.lookup r.node r.model.nodes of
      Just n -> rebindRestored now t r.node n r.model
      Nothing -> openFresh now t r.model -- queued node vanished; consume the slot, open fresh
    Nothing -> openFresh now t model

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

-- | A restored closed tab re-using its existing node (no duplicate). Position in
-- | the tree is unchanged; the node just goes Live again. The pending-restore
-- | queue was already advanced by `popPendingRestore`.
rebindRestored :: Number -> OpenedTab -> NodeId -> Node -> Model -> Step
rebindRestored _ t _ n model =
  let
    n' = n
      { tabId = Just t.tabId
      , active = t.active
      , title = t.title
      , url = t.url
      , favIconUrl = t.favIconUrl
      , closedAt = Nothing
      }
    patch = { upserts: [ n' ], removes: [], roots: Nothing }
  in
    commit model.nextId patch model

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
