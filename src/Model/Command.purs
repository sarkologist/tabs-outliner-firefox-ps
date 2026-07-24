-- | The user-command half of the reducer: pure `Model -> {model, patch,
-- | actions}`. `actions` are the browser-side effects a command implies (focus,
-- | create, remove) — produced purely here and interpreted by the background,
-- | so the reducer stays fully testable. Also defines the tiny request protocol
-- | (GetSnapshot | RunCommand) the channel carries.
module Model.Command where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Set as Set
import Data.String (Pattern(..), stripPrefix)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..))
import Model.Codec (Snapshot, decodeSnapshot, encodeSnapshotData)
import Model.Tree (applyPatch, insertAtClamped, isAncestorOrSelf, liveTabCountInWindow, liveWindowNode, mergePatch, ownedTabPreorder, owningGroupAncestor, pruneFrom, rootAncestor, subtreeIds)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, PendingWindow, defaultNode, emptyPatch, isLiveTab)

data Command
  = Collapse NodeId Boolean
  | ExpandAncestors NodeId
  | Rename NodeId String
  | Activate NodeId -- focus a live tab, or restore a closed one
  | CloseNode NodeId -- close the live tabs in the subtree (keep history)
  | Delete NodeId -- remove the subtree from the tree (and close its live tabs)
  | Move NodeId (Maybe NodeId) Int -- node, new parent (Nothing = root), index
  | MoveTopLevel NodeId -- pull a nested node out to the root, just after its root ancestor
  | MoveBottom NodeId -- pull a node out to the very bottom of the root list
  | Flatten NodeId -- dissolve a group, promoting its children
  | Group NodeId -- wrap a node in a group/window
  | Import Snapshot -- add an exported outline as inert, restorable top-level nodes
  | Drop NodeId NodeId -- drag dragId onto targetId; resolved here to a Move
  | PasteAfter NodeId NodeId -- cut/paste source after target; resolved here to a Move

-- | Browser-side effects a command implies (interpreted by the background).
-- | `CreateWindow` opens one new browser window populated with the given urls
-- | (so restoring a closed window re-creates it as its own window, not as tabs
-- | dumped into whatever window is currently focused). `MoveTabToWindow` and
-- | `NewWindowWithTabs` carry a live-tab reorganization through to the real
-- | browser: when the user drags a live tab (or flattens a live window) to a new
-- | owning container, the actual tab(s) move — into that container's window, or a
-- | fresh one holding them all — and the tree re-settles from the resulting
-- | onAttached/onCreated events.
data BrowserAction
  = FocusTab Int
  -- | Open a tab: window, index, url, and — when the restore queued a node to
  -- | rebind in that window — that node. A rejected create can then retract
  -- | exactly its own queue entry (`TabCreateFailed`); leaving it queued would let
  -- | it hijack the next tab to open in that window, since the queue is a FIFO
  -- | matched by creation order.
  | CreateTab (Maybe Int) (Maybe Int) (Maybe String) (Maybe NodeId)
  -- | Open one new browser window for the given urls, binding it to container
  -- | `NodeId` when it opens (the impure layer pairs the two, so the restore
  -- | can't cross-wire to another in-flight restore's window).
  | CreateWindow NodeId (Array String)
  | MoveTabToWindow Int Int Int -- tabId, destination (live) windowId, index (-1 = append)
  -- | Detach these tabs into one brand-new window. `Just node` when a saved/plain
  -- | container "goes live" as that window (bind it on open); `Nothing` for a
  -- | fresh window at the root (no container to bind).
  | NewWindowWithTabs (Maybe NodeId) (Array Int)
  | RemoveTab Int

derive instance eqBrowserAction :: Eq BrowserAction
instance showBrowserAction :: Show BrowserAction where
  show (FocusTab t) = "FocusTab " <> show t
  show (CreateTab w i u n) = "CreateTab " <> show w <> " " <> show i <> " " <> show u <> " " <> show n
  show (CreateWindow n us) = "CreateWindow " <> show n <> " " <> show us
  show (MoveTabToWindow t w i) = "MoveTabToWindow " <> show t <> " " <> show w <> " " <> show i
  show (NewWindowWithTabs n ts) = "NewWindowWithTabs " <> show n <> " " <> show ts
  show (RemoveTab t) = "RemoveTab " <> show t

-- | Where a restored tab should reopen, decided by its direct group parent.
-- | Tabs nested under tabs do not inherit a group/window owner.
data RestoreTarget
  = IntoWindow Int -- parent already live as a window (reopen the tab back into it)
  | IntoNewWindow NodeId -- saved-container parent (its tabs open one new window it goes live as)
  | IntoCurrent -- no parent container (reopen in the current window)

derive instance eqRestoreTarget :: Eq RestoreTarget

type CmdResult = { model :: Model, patch :: Patch, actions :: Array BrowserAction }

groupTitle :: String
groupTitle = "Group"

-- | Run a command, then restore the invariant that a tab never sits bare at the
-- | root (every KTab has a container parent). Wrapping is centralized here so it
-- | covers every way a tab can reach the root — a move/drag/move-to-top/bottom, a
-- | flatten of a root group, an import of the original's portable format — and so a
-- | restore always routes a tab through a window/group (never the unflagged
-- | reopen-into-current path that a parentless tab would have taken).
applyCommand :: Number -> Command -> Model -> CmdResult
applyCommand now cmd model = wrapRootTabs now (applyCommandRaw now cmd model)

-- | A tab that ended up at the root is wrapped in a fresh group in place.
wrapRootTabs :: Number -> CmdResult -> CmdResult
wrapRootTabs now r =
  let w = wrapRootTabsModel now r.model
  in r { model = w.model, patch = mergePatch r.patch w.patch }

-- | Wrap every root-level CLOSED tab in a fresh group (the no-bare-root-tab
-- | invariant). A LIVE tab momentarily at the root is left alone — e.g. flattening a
-- | top-level live window promotes its tabs to root, but a browser action
-- | (`NewWindowWithTabs`) is already moving them into a new window, and the resulting
-- | events re-home them; wrapping them would spawn stray groups. Cheap: scans only
-- | the root list, allocates only when a closed tab actually reached it. Returns the
-- | new model and the patch (empty if nothing wrapped); reused at boot to normalize
-- | loaded data so a pre-existing bare root tab is fixed before its first restore.
wrapRootTabsModel :: Number -> Model -> { model :: Model, patch :: Patch }
wrapRootTabsModel now model =
  let
    wrap a rootId = case Map.lookup rootId model.nodes of
      Just n | n.kind == KTab && not (isLiveTab n) ->
        let g = (defaultNode ("n" <> show a.nid) KGroup now) { title = groupTitle, children = [ rootId ] }
        in { roots: Array.snoc a.roots g.id, ups: a.ups <> [ g, n { parent = Just g.id } ], nid: a.nid + 1 }
      _ -> a { roots = Array.snoc a.roots rootId }
    acc = foldl wrap { roots: [], ups: [], nid: model.nextId } model.roots
  in
    if acc.nid == model.nextId then { model, patch: emptyPatch }
    else
      let patch = { upserts: acc.ups, removes: [], roots: Just acc.roots }
      in { model: (applyPatch patch model) { nextId = acc.nid }, patch }

applyCommandRaw :: Number -> Command -> Model -> CmdResult
applyCommandRaw now cmd model = case cmd of
  Collapse nid value -> withNode nid \n -> upsertOnly (n { collapsed = value })

  ExpandAncestors nid -> withNode nid \n ->
    let
      upserts = ancestorUpserts n.parent
      patch = { upserts, removes: [], roots: Nothing }
    in
      { model: applyPatch patch model, patch, actions: [] }

  Rename nid title -> withNode nid \n -> upsertOnly (n { customTitle = Just title })

  Activate nid -> withNode nid \n -> case n.tabId of
    Just t -> actionsOnly [ FocusTab t ]
    Nothing -> restore nid

  -- "Close (keep history)": remove the live tabs in the subtree but keep their
  -- nodes as closed history. The browser reports each removal as a plain
  -- tabs.onRemoved, indistinguishable from a user closing the tab — so mark these
  -- tabIds as outliner-initiated, letting Model.Reconcile keep them (even a
  -- restored tab, which a *browser* close would instead drop).
  CloseNode nid ->
    let tabIds = ownedLiveTabIds nid
    in
      { model: model { closingTabs = Set.union model.closingTabs (Set.fromFoldable tabIds) }
      , patch: emptyPatch
      , actions: map RemoveTab tabIds
      }

  Delete nid -> case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node ->
      let
        ids = subtreeIds nid model
        parentUpserts = detachUpserts node
        rootsM = if Array.elem nid model.roots then Just (Array.delete nid model.roots) else Nothing
        patch = { upserts: parentUpserts, removes: ids, roots: rootsM }
      in
        withPrune node.parent
          { model: applyPatch patch model, patch, actions: map RemoveTab (subtreeLiveTabIds nid) }

  Move nid mParent index -> move nid mParent index

  -- "Move to top level" pulls a nested node out to the root, landing it just after
  -- the root it currently belongs to (matching the original). Works on any kind: a
  -- live tab can't sit bare at the root, so — exactly like dragging one there — it's
  -- promoted into its own new window (the `move` path turns that into a browser
  -- action); non-live nodes just move within the tree.
  MoveTopLevel nid -> withNode nid \n -> case n.parent of
    Nothing -> noChange -- already top level
    Just _ -> case Array.elemIndex (rootAncestor nid model) model.roots of
      Just ri -> move nid Nothing (ri + 1)
      Nothing -> noChange

  -- "Move to bottom" sends a node to the very end of the root list (a no-op if it's
  -- already the last root). Same per-kind handling as MoveTopLevel.
  MoveBottom nid -> withNode nid \_ ->
    if Array.last model.roots == Just nid then noChange
    else move nid Nothing (Array.length model.roots)

  Flatten nid -> flatten nid

  Group nid -> groupNode nid

  Import snap ->
    let
      count = Array.length snap.nodes
      idMap = Map.fromFoldable
        (Array.mapWithIndex (\i n -> Tuple n.id ("n" <> show (model.nextId + i))) snap.nodes)
      remap old = Map.lookup old idMap
      -- imported nodes are inert history: every browser binding is dropped, so
      -- tabs/windows become restorable and containers plain saved groups, and
      -- references outside the imported set are dropped (never aliased onto live
      -- nodes).
      remapNode n = n
        { id = fromMaybe n.id (remap n.id)
        , parent = n.parent >>= remap
        , children = Array.mapMaybe remap n.children
        , tabId = Nothing
        , windowId = Nothing
        , active = false
        }
      remapped = map remapNode snap.nodes
      patch = { upserts: remapped, removes: [], roots: Just (model.roots <> Array.mapMaybe remap snap.roots) }
      model' = (applyPatch patch model) { nextId = model.nextId + count }
    in
      { model: model', patch, actions: [] }

  -- A drop onto a group lands as its last child; onto anything else, immediately
  -- before the target as a sibling (the index accounts for the dragged node's own
  -- removal, so a same-parent downward drag lands before the target). `move` does
  -- the cycle guard, live-tab routing, and prune. The sidebar's drop preview
  -- (Model.Drop.dropPlacement) is the visual twin of this.
  Drop dragId targetId
    | dragId == targetId -> noChange
    | otherwise -> case Map.lookup targetId model.nodes of
      Nothing -> noChange
      Just target
        | target.kind == KGroup -> move dragId (Just target.id) (Array.length target.children)
        | otherwise ->
            let
              siblings = case target.parent of
                Just pid -> fromMaybe [] (_.children <$> Map.lookup pid model.nodes)
                Nothing -> model.roots
              shrunk = Array.delete dragId siblings
              idx = fromMaybe (Array.length shrunk) (Array.elemIndex targetId shrunk)
            in
              move dragId target.parent idx

  -- Cut/paste uses the old extension's semantics: paste the cut subtree AFTER
  -- the target row, never inside it. The move path below keeps pruning, root-tab
  -- wrapping, live-tab browser routing, and undo behavior identical to drag/reorder.
  PasteAfter sourceId targetId
    | sourceId == targetId -> noChange
    | otherwise -> case Map.lookup targetId model.nodes of
      Nothing -> noChange
      Just target ->
        let
          siblings = case target.parent of
            Just pid -> fromMaybe [] (_.children <$> Map.lookup pid model.nodes)
            Nothing -> model.roots
          shrunk = Array.delete sourceId siblings
          idx = maybe (Array.length shrunk) (_ + 1) (Array.elemIndex targetId shrunk)
        in
          move sourceId target.parent idx
  where
  noChange :: CmdResult
  noChange = { model, patch: emptyPatch, actions: [] }

  withNode :: NodeId -> (Node -> CmdResult) -> CmdResult
  withNode nid f = maybe noChange f (Map.lookup nid model.nodes)

  upsertOnly :: Node -> CmdResult
  upsertOnly n =
    let patch = { upserts: [ n ], removes: [], roots: Nothing }
    in { model: applyPatch patch model, patch, actions: [] }

  actionsOnly :: Array BrowserAction -> CmdResult
  actionsOnly actions = { model, patch: emptyPatch, actions }

  ancestorUpserts :: Maybe NodeId -> Array Node
  ancestorUpserts = go Set.empty []
    where
    go seen acc = case _ of
      Nothing -> acc
      Just pid
        | Set.member pid seen -> acc
        | otherwise -> case Map.lookup pid model.nodes of
            Nothing -> acc
            Just p ->
              let
                acc' = if p.collapsed then Array.snoc acc (p { collapsed = false }) else acc
              in
                go (Set.insert pid seen) acc' p.parent

  -- After an edit detached a child from `mParent`, prune that parent if it is now a
  -- childless, un-renamed group (cascading up), folding the removal into the edit's
  -- patch. (A live-tab move detaches later, via the browser event, so it prunes in
  -- Model.Reconcile instead.)
  withPrune :: Maybe NodeId -> CmdResult -> CmdResult
  withPrune mParent r = case mParent of
    Nothing -> r
    Just pid -> let p = pruneFrom pid r.model in { model: p.model, patch: mergePatch r.patch p.patch, actions: r.actions }

  -- live tab ids owned by nid, walking through restored tab nesting but not into
  -- nested groups/windows.
  ownedLiveTabIds :: NodeId -> Array Int
  ownedLiveTabIds nid =
    Array.mapMaybe (\i -> Map.lookup i model.nodes >>= _.tabId) (ownedTabPreorder model nid)

  -- live tab ids in the whole subtree, for destructive delete.
  subtreeLiveTabIds :: NodeId -> Array Int
  subtreeLiveTabIds nid = Array.mapMaybe
    (\i -> Map.lookup i model.nodes >>= _.tabId)
    (subtreeIds nid model)

  -- upsert that removes nid from whatever parent currently holds it (window/group)
  detachUpserts :: Node -> Array Node
  detachUpserts node = case node.parent of
    Just pid -> case Map.lookup pid model.nodes of
      Just p -> [ p { children = Array.delete node.id p.children } ]
      Nothing -> []
    Nothing -> []

  -- restore: re-open this closed tab and its tab descendants, or the owned tab
  -- descendants of this group, re-binding to existing nodes via pendingRestore
  -- (keyed by the window each tab is recreated in) when each onCreated arrives.
  -- Tab descendants inherit the nearest group/window ancestor as runtime owner,
  -- but nested groups/windows are separate restore boundaries.
  restore :: NodeId -> CmdResult
  restore nid =
    let
      queuedTabs = queuedRestoreTabs model
      queuedWindows = Set.fromFoldable (map _.node model.pendingRestoreWindows)
      closedTabs = closedOwnedTabs model nid
      -- only tabs with a url the browser will actually open can be reopened; keep
      -- subtree (preorder) order. Skipping an un-openable url (file:, about:, …)
      -- matters because a window batches all its tabs into one windows.create,
      -- which the browser rejects WHOLE if any url is disallowed — so one file://
      -- tab would otherwise silently doom the entire window restore. The skipped
      -- tab stays as closed history in place.
      tagged = Array.mapMaybe
        ( \n ->
            if Set.member n.id queuedTabs then Nothing
            else case n.url of
              Just u | restorableUrl u -> Just { id: n.id, url: u, target: restoreTargetOf model n.id }
              _ -> Nothing
        )
        closedTabs
      -- If a saved group/window is already waiting for its browser window, a second
      -- click before events settle must not create a duplicate window. The user can
      -- restore more from that group once the first window binds.
      ready = Array.filter (\x -> case x.target of
        IntoNewWindow w -> not (Set.member w queuedWindows)
        _ -> true) tagged

      -- one new window per closed-window ancestor, in first-seen order
      newWinIds = Array.nub (Array.mapMaybe (\x -> case x.target of
        IntoNewWindow w -> Just w
        _ -> Nothing) ready)
      forWindow w = Array.filter (\x -> x.target == IntoNewWindow w) ready
      windowActions = map (\w -> CreateWindow w (map _.url (forWindow w))) newWinIds
      -- carry the EXACT node ids (same order as the urls above) so each rebinds to
      -- the right node when the window's tabs arrive — not "all of the container's
      -- closed children", which a partial restore must not resurrect.
      newWindows = map (\w -> { node: w, tabs: List.fromFoldable (map _.id (forWindow w)) }) newWinIds

      tabActions = Array.mapMaybe (\x -> case x.target of
        -- carries x.id: this tab IS queued below, so a rejected create must be
        -- able to un-queue exactly it
        IntoWindow wid -> Just (CreateTab (Just wid) (restoreIndex wid x.id) (Just x.url) (Just x.id))
        -- IntoCurrent queues nothing (no window to key it by), so nothing to retract
        IntoCurrent -> Just (CreateTab Nothing Nothing (Just x.url) Nothing)
        IntoNewWindow _ -> Nothing) ready

      -- queue each IntoWindow tab under its target window — a FIFO consumed as the
      -- recreated tabs' onCreated events arrive (in this same order). IntoNewWindow
      -- tabs are queued when their window opens (Model.Reconcile); IntoCurrent tabs
      -- can't be pre-keyed by a window, so they just reopen as fresh nodes.
      queueIntoWindow m x = case x.target of
        IntoWindow wid -> Map.alter (\ml -> Just (maybe (List.singleton x.id) (\l -> List.snoc l x.id) ml)) wid m
        _ -> m
      pending' = foldl queueIntoWindow model.pendingRestore ready

      -- tabs.create's index is counted among live browser tabs. To keep restores
      -- in saved tree order, count siblings before this node that are already live
      -- plus siblings queued by this or an earlier not-yet-reconciled restore.
      restoreIndex :: Int -> NodeId -> Maybe Int
      restoreIndex wid id = do
        w <- liveWindowNode wid model
        let
          queued = maybe [] Array.fromFoldable (Map.lookup wid model.pendingRestore)
          restoring = Set.fromFoldable
            (queued <> map _.id (Array.filter (\x -> x.target == IntoWindow wid) ready))
          counts n = isLiveTab n || Set.member n.id restoring
          ordered = Array.filter
            (\cid -> maybe false counts (Map.lookup cid model.nodes))
            (ownedTabPreorder model w.id)
        Array.elemIndex id ordered

      -- Mark every closed tab we are reopening so a later *browser* close keeps it as
      -- history (a restored tab belongs in the tree), whereas a freshly-opened tab is
      -- dropped (Reconcile.TabClosed). The flag is set HERE — where we know a genuine
      -- user restore is happening — and not in `rebindRestored`, because a live tab
      -- rehomed into a saved group also rebinds via `pendingRestore`; flagging at the
      -- rebind would mistake that (and any later tab in that window) for a restore.
      flagged = Array.mapMaybe
        (\x -> (\n -> n { restoredFromClosed = true }) <$> Map.lookup x.id model.nodes) ready
      patch = { upserts: flagged, removes: [], roots: Nothing }
      model' = (applyPatch patch model)
        { pendingRestore = pending'
        , pendingRestoreWindows = model.pendingRestoreWindows <> newWindows
        }
    in
      { model: model'
      , patch
      , actions: windowActions <> tabActions
      }

  groupNode :: NodeId -> CmdResult
  groupNode nid = case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node ->
      let
        gid = "n" <> show model.nextId
        g = (defaultNode gid KGroup now) { title = groupTitle, parent = node.parent, children = [ nid ] }
        node' = node { parent = Just gid }
        parentUpserts = case node.parent >>= (\pid -> Map.lookup pid model.nodes) of
          Just p -> [ p { children = spliceReplace nid [ gid ] p.children } ]
          Nothing -> []
        rootsM = if Array.elem nid model.roots then Just (spliceReplace nid [ gid ] model.roots) else Nothing
        patch = { upserts: [ g, node' ] <> parentUpserts, removes: [], roots: rootsM }
        model' = (applyPatch patch model) { nextId = model.nextId + 1 }
      in
        case node.tabId of
          Just t ->
            { model: model' { pendingRestoreWindows = pushPending gid model'.pendingRestoreWindows }
            , patch
            , actions: [ NewWindowWithTabs (Just gid) [ t ] ]
            }
          Nothing -> { model: model', patch, actions: [] }

  move :: NodeId -> Maybe NodeId -> Int -> CmdResult
  move nid mParent index = case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node
      -- reject a move into the node's own subtree (O(depth) upward walk)
      | mParent == Just nid || maybe false (\p -> isAncestorOrSelf nid p model) mParent -> noChange
      -- a live tab move must drive the real browser tab, even within the same
      -- window; the tree re-settles from tabs.onMoved/onAttached so live child
      -- order stays the browser's tab order.
      | isLiveTab node -> moveLiveTab node mParent index
      | otherwise ->
          let
            detached = detachUpserts node
            rootsDetached = Array.delete nid model.roots
            node' = node { parent = mParent }
            result = case mParent of
              Nothing ->
                let patch = { upserts: detached <> [ node' ], removes: [], roots: Just (insertAtClamped index nid rootsDetached) }
                in { model: applyPatch patch model, patch, actions: [] }
              Just pid -> case Map.lookup pid model.nodes of
                Nothing -> noChange
                Just p0 ->
                  let
                    -- when reordering within the same parent, start from the detached children
                    base = if node.parent == Just pid then Array.delete nid p0.children else p0.children
                    pNew = p0 { children = insertAtClamped index nid base }
                    upserts = (if node.parent == Just pid then [] else detached) <> [ pNew, node' ]
                    rootsM = if Array.elem nid model.roots then Just rootsDetached else Nothing
                    patch = { upserts, removes: [], roots: rootsM }
                  in
                    { model: applyPatch patch model, patch, actions: [] }
          -- moving the node out may have emptied its old parent
          in withPrune node.parent result

  -- A live tab dragged to a different owning container: move the REAL browser tab,
  -- not the tree node. The tree re-settles from the resulting browser events.
  moveLiveTab :: Node -> Maybe NodeId -> Int -> CmdResult
  moveLiveTab node mParent index = case node.tabId of
    Nothing -> noChange -- unreachable under the isLiveTab guard; keeps this total
    Just t -> case mParent of
      Just pid | Just parent <- Map.lookup pid model.nodes, Just w <- parent.windowId ->
        -- UI moves are expressed as child-array slots; tabs.move expects a live-tab
        -- slot. Remove the moving node first so same-window reorders use the
        -- post-detach coordinates that the browser will see.
        actionsOnly [ MoveTabToWindow t w (liveSlotAfterDetach node parent index) ]
      -- new-window cases (a plain container goes live, or out to the root)
      _ -> let r = rehome model mParent [ t ] in { model: r.model, patch: emptyPatch, actions: r.actions }

  liveSlotAfterDetach :: Node -> Node -> Int -> Int
  liveSlotAfterDetach moving parent index =
    let
      base = if moving.parent == Just parent.id then Array.delete moving.id parent.children else parent.children
      slot = clamp 0 (Array.length base) index
    in
      foldl (\n cid -> n + liveTabCountInWindow model parent.id cid) 0 (Array.take slot base)

  -- Browser action(s) to re-home live `tabIds` to container `mParent` (their new
  -- owning window): into an already-live window -> move each there; into a
  -- not-yet-live container -> queue it to bind one new window holding them all
  -- (it "goes live", rebinding on onCreated like a restore); to the root -> a
  -- fresh window holding them all.
  rehome :: Model -> Maybe NodeId -> Array Int -> { model :: Model, actions :: Array BrowserAction }
  rehome m mParent tabIds
    | Array.null tabIds = { model: m, actions: [] }
    | otherwise = case mParent of
        Nothing -> { model: m, actions: [ NewWindowWithTabs Nothing tabIds ] }
        Just pid -> case Map.lookup pid m.nodes of
          Just p | Just w <- p.windowId -> { model: m, actions: map (\t -> MoveTabToWindow t w (-1)) tabIds }
          -- de-dupe the queue so two drags into the same not-yet-live container
          -- can't both pop a window and double-bind it
          Just _ -> { model: m { pendingRestoreWindows = pushPending pid m.pendingRestoreWindows }, actions: [ NewWindowWithTabs (Just pid) tabIds ] }
          Nothing -> { model: m, actions: [] }

  flatten :: NodeId -> CmdResult
  flatten nid = case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node
      | node.kind /= KGroup -> noChange -- only dissolve containers (groups/windows), never tabs
      | otherwise ->
          let
            kids = node.children
            -- live tabs being promoted change their owning window from `node` to
            -- its parent, so the real browser tabs move there too: flattening a
            -- live window re-homes its tabs (a plain group has none to move).
            kidTabIds = Array.mapMaybe (\cid -> Map.lookup cid model.nodes >>= _.tabId) kids
            promote parentRef = Array.mapMaybe
              (\cid -> (\c -> c { parent = parentRef }) <$> Map.lookup cid model.nodes)
              kids
            -- Flatten preserves the dissolved window's position in its parent.
            -- If the parent is already a live browser window, move the real tabs
            -- to that same live index instead of appending them.
            rehomeFlatten m = case node.parent of
              Just pid | Just p <- Map.lookup pid model.nodes, Just w <- p.windowId ->
                let
                  before = Array.takeWhile (_ /= nid) p.children
                  baseIndex = foldl (\n cid -> n + liveTabCountInWindow model p.id cid) 0 before
                in
                  { model: m, actions: Array.mapWithIndex (\i t -> MoveTabToWindow t w (baseIndex + i)) kidTabIds }
              _ -> rehome m node.parent kidTabIds
            withBrowser patch =
              let br = rehomeFlatten (applyPatch patch model)
              in { model: br.model, patch, actions: br.actions }
          in
            case node.parent of
              Just pid -> case Map.lookup pid model.nodes of
                Nothing -> noChange
                Just p -> withPrune node.parent (withBrowser { upserts: [ p { children = spliceReplace nid kids p.children } ] <> promote (Just pid), removes: [ nid ], roots: Nothing })
              Nothing -> withBrowser { upserts: promote Nothing, removes: [ nid ], roots: Just (spliceReplace nid kids model.roots) }

-- | Every tab node already awaiting a restore — queued into a live window, or
-- | carried by a container still waiting for its browser window. Shared by
-- | `restore` (which must not re-issue them) and `unopenableOnRestore` (which must
-- | not count an in-flight tab as one the browser refused).
queuedRestoreTabs :: Model -> Set.Set NodeId
queuedRestoreTabs model = Set.fromFoldable
  ( Array.concatMap Array.fromFoldable (Array.fromFoldable (Map.values model.pendingRestore) :: Array (List NodeId))
      <> Array.concatMap (Array.fromFoldable <<< _.tabs) model.pendingRestoreWindows
  )

-- | The closed tab nodes a restore of `nid` would reopen, in subtree preorder.
closedOwnedTabs :: Model -> NodeId -> Array Node
closedOwnedTabs model nid =
  Array.mapMaybe
    ( \cid -> case Map.lookup cid model.nodes of
        Just c | not (isLiveTab c) -> Just c
        _ -> Nothing
    )
    (ownedTabPreorder model nid)

-- | How many closed tabs an `Activate` will leave behind because the browser
-- | refuses their url. Computed from the PRE-command model (like
-- | `pasteSourceMissing`), so `CmdResult` keeps its shape.
-- |
-- | This exists because the honest answer to "why did nothing happen?" is
-- | otherwise unavailable: clicking a container whose every closed tab is
-- | un-openable — a window of `file://` pages, say — produces no patch and no
-- | browser action, so the click is indistinguishable from a broken build. The
-- | sidebar turns this count into a notice.
unopenableOnRestore :: Command -> Model -> Int
unopenableOnRestore (Activate nid) model = case Map.lookup nid model.nodes of
  Just n | isNothing n.tabId ->
    let queued = queuedRestoreTabs model
    in Array.length
      ( Array.filter
          (\c -> not (Set.member c.id queued) && not (maybe false restorableUrl c.url))
          (closedOwnedTabs model nid)
      )
  _ -> 0
unopenableOnRestore _ _ = 0

spliceReplace :: NodeId -> Array NodeId -> Array NodeId -> Array NodeId
spliceReplace x ys = Array.concatMap (\e -> if e == x then ys else [ e ])

-- Append a container to the pending-window queue unless it is already waiting. A
-- rehome carries no tabs to rebind (the dragged tab arrives via onAttached); the
-- container just needs to bind the new window.
pushPending :: NodeId -> Array PendingWindow -> Array PendingWindow
pushPending pid xs = if Array.any (\e -> e.node == pid) xs then xs else Array.snoc xs { node: pid, tabs: Nil }

-- | Schemes a WebExtension can't open in a tab: `windows.create`/`tabs.create`
-- | reject them, and since a window restore batches every tab into one
-- | `windows.create`, a single rejected url fails the WHOLE window (no window
-- | appears). `file:` needs a user-granted file-URL access this add-on doesn't
-- | request; the rest are privileged/internal/opaque. A tab with such a url is
-- | left as closed history rather than restored.
-- |
-- | The `*-extension:` entries matter in practice: an imported Chrome Tabs
-- | Outliner tree carries `chrome-extension:` pages that Firefox can never open,
-- | and `moz-extension:` pages belong to a specific add-on install (another
-- | add-on's, or a stale uuid of ours), so the browser rejects them too. Before
-- | they were filtered, one such tab silently doomed the restore of every other
-- | tab sharing its window.
-- |
-- | `moz-extension:` is blocked wholesale, which also skips a saved tab pointing at
-- | THIS add-on's own options page — the one such url the browser would accept.
-- | Deliberate: telling the two apart needs the live extension origin, which this
-- | pure reducer has no access to, and a persisted moz-extension url is usually
-- | stale anyway (the uuid is per-install, so it dies on reinstall). Skipping one
-- | marginal own-page restore beats letting any of them doom a whole window.
blockedSchemes :: Array String
blockedSchemes =
  [ "file:"
  , "about:"
  , "chrome:"
  , "resource:"
  , "javascript:"
  , "view-source:"
  , "data:"
  , "moz-extension:"
  , "chrome-extension:"
  , "blob:"
  , "filesystem:"
  , "jar:"
  ]

restorableUrl :: String -> Boolean
restorableUrl u = let lu = toLower u in not (Array.any (\p -> isJust (stripPrefix (Pattern p) lu)) blockedSchemes)

-- | Where a closed tab node should reopen. The nearest group/window ancestor owns
-- | the runtime window, walking through tab parents but not across group
-- | boundaries. A live group -> back into that window; a saved group -> a new
-- | window that the group goes live as; no group parent -> the current window.
restoreTargetOf :: Model -> NodeId -> RestoreTarget
restoreTargetOf model nid = case owningGroupAncestor model nid of
  Just p
    | Just wid <- p.windowId -> IntoWindow wid
    | otherwise -> IntoNewWindow p.id
  Nothing -> IntoCurrent

-- Request protocol -----------------------------------------------------------

-- A window of the visible order: rows [start, start+count) of the order for
-- `query`, with the active tab's index in `myWindow` when `wantFocus`. With
-- `tail`, `start` is ignored and the *last* window is returned (the open default,
-- since new windows land at the bottom — that's where the live nodes are).
type ViewReq =
  { start :: Int
  , count :: Int
  , query :: String
  , myWindow :: Maybe Int
  , wantFocus :: Boolean
  , tail :: Boolean
  , targetNodeId :: Maybe NodeId
  }

data Request
  = GetView ViewReq
  | RunCommand Command
  | Undo
  | Redo
  | Export
  | OpenFullSizeOutliner (Maybe Int)
  | GetAutomaticBackups
  | SetAutomaticBackups Boolean

encodeRequest :: Request -> Json
encodeRequest (GetView r) = encodeJson
  { tag: "getView", start: r.start, count: r.count, query: r.query, myWindow: r.myWindow, wantFocus: r.wantFocus, tail: r.tail, targetNodeId: r.targetNodeId }
encodeRequest (RunCommand c) = encodeJson { tag: "command", body: encodeCommand c }
encodeRequest Undo = encodeJson { tag: "undo" }
encodeRequest Redo = encodeJson { tag: "redo" }
encodeRequest Export = encodeJson { tag: "export" }
encodeRequest (OpenFullSizeOutliner sourceWindowId) = encodeJson { tag: "openFullSizeOutliner", sourceWindowId }
encodeRequest GetAutomaticBackups = encodeJson { tag: "getAutomaticBackups" }
encodeRequest (SetAutomaticBackups enabled) = encodeJson { tag: "setAutomaticBackups", enabled }

decodeRequest :: Json -> Either String Request
decodeRequest json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "getView" -> case (dec json :: Either String ViewReq) of
      Right r -> Right (GetView r)
      Left _ -> do
        r <- dec json :: Either String
          { start :: Int
          , count :: Int
          , query :: String
          , myWindow :: Maybe Int
          , wantFocus :: Boolean
          , tail :: Boolean
          }
        Right (GetView
          { start: r.start
          , count: r.count
          , query: r.query
          , myWindow: r.myWindow
          , wantFocus: r.wantFocus
          , tail: r.tail
          , targetNodeId: Nothing
          })
    "command" -> do
      { body } <- dec json :: Either String { body :: Json }
      RunCommand <$> decodeCommand body
    "undo" -> Right Undo
    "redo" -> Right Redo
    "export" -> Right Export
    "openFullSizeOutliner" -> case (dec json :: Either String { sourceWindowId :: Maybe Int }) of
      Right r -> Right (OpenFullSizeOutliner r.sourceWindowId)
      Left _ -> Right (OpenFullSizeOutliner Nothing)
    "getAutomaticBackups" -> Right GetAutomaticBackups
    "setAutomaticBackups" -> (\r -> SetAutomaticBackups r.enabled) <$> (dec json :: Either String { enabled :: Boolean })
    other -> Left ("unknown request: " <> other)

encodeCommand :: Command -> Json
encodeCommand = case _ of
  Collapse nid value -> encodeJson { tag: "collapse", id: nid, value }
  ExpandAncestors nid -> encodeJson { tag: "expandAncestors", id: nid }
  Rename nid title -> encodeJson { tag: "rename", id: nid, title }
  Activate nid -> encodeJson { tag: "activate", id: nid }
  CloseNode nid -> encodeJson { tag: "close", id: nid }
  Delete nid -> encodeJson { tag: "delete", id: nid }
  Move nid parent index -> encodeJson { tag: "move", id: nid, parent, index }
  MoveTopLevel nid -> encodeJson { tag: "moveTopLevel", id: nid }
  MoveBottom nid -> encodeJson { tag: "moveBottom", id: nid }
  Flatten nid -> encodeJson { tag: "flatten", id: nid }
  Group nid -> encodeJson { tag: "group", id: nid }
  Import snap -> encodeJson { tag: "import", body: encodeSnapshotData snap }
  Drop drag target -> encodeJson { tag: "drop", drag, target }
  PasteAfter source target -> encodeJson { tag: "pasteAfter", source, target }

decodeCommand :: Json -> Either String Command
decodeCommand json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "collapse" -> (\r -> Collapse r.id r.value) <$> (dec json :: Either String { id :: NodeId, value :: Boolean })
    "expandAncestors" -> (\r -> ExpandAncestors r.id) <$> (dec json :: Either String { id :: NodeId })
    "rename" -> (\r -> Rename r.id r.title) <$> (dec json :: Either String { id :: NodeId, title :: String })
    "activate" -> (\r -> Activate r.id) <$> (dec json :: Either String { id :: NodeId })
    "close" -> (\r -> CloseNode r.id) <$> (dec json :: Either String { id :: NodeId })
    "delete" -> (\r -> Delete r.id) <$> (dec json :: Either String { id :: NodeId })
    "move" -> (\r -> Move r.id r.parent r.index) <$> (dec json :: Either String { id :: NodeId, parent :: Maybe NodeId, index :: Int })
    "moveTopLevel" -> (\r -> MoveTopLevel r.id) <$> (dec json :: Either String { id :: NodeId })
    "moveBottom" -> (\r -> MoveBottom r.id) <$> (dec json :: Either String { id :: NodeId })
    "flatten" -> (\r -> Flatten r.id) <$> (dec json :: Either String { id :: NodeId })
    "group" -> (\r -> Group r.id) <$> (dec json :: Either String { id :: NodeId })
    "import" -> do
      { body } <- dec json :: Either String { body :: Json }
      Import <$> decodeSnapshot body
    "drop" -> (\r -> Drop r.drag r.target) <$> (dec json :: Either String { drag :: NodeId, target :: NodeId })
    "pasteAfter" -> (\r -> PasteAfter r.source r.target) <$> (dec json :: Either String { source :: NodeId, target :: NodeId })
    other -> Left ("unknown command: " <> other)

dec :: forall a. DecodeJson a => Json -> Either String a
dec = lmap printJsonDecodeError <<< decodeJson
