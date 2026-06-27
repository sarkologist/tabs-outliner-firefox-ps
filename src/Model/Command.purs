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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Model.Codec (Snapshot, decodeSnapshot, encodeSnapshotData)
import Model.Tree (applyPatch, insertAtClamped, isAncestorOrSelf, mergePatch, pruneFrom, rootAncestor, subtreeIds)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, PendingWindow, defaultNode, emptyPatch, isLiveTab)

data Command
  = Collapse NodeId Boolean
  | Rename NodeId String
  | Activate NodeId -- focus a live tab, or restore a closed one
  | CloseNode NodeId -- close the live tabs in the subtree (keep history)
  | Delete NodeId -- remove the subtree from the tree (and close its live tabs)
  | Move NodeId (Maybe NodeId) Int -- node, new parent (Nothing = root), index
  | MoveTopLevel NodeId -- pull a nested node out to the root, just after its root ancestor
  | MoveBottom NodeId -- pull a node out to the very bottom of the root list
  | Flatten NodeId -- dissolve a group, promoting its children
  | NewGroup (Maybe NodeId) Int -- new folder under parent at index
  | Import Snapshot -- add an exported outline as inert, restorable top-level nodes
  | Drop NodeId NodeId -- drag dragId onto targetId; resolved here to a Move

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
  | CreateTab (Maybe Int) (Maybe String)
  | CreateWindow (Array String)
  | MoveTabToWindow Int Int Int -- tabId, destination (live) windowId, index (-1 = append)
  | NewWindowWithTabs (Array Int) -- detach these tabs into one brand-new window
  | RemoveTab Int

derive instance eqBrowserAction :: Eq BrowserAction
instance showBrowserAction :: Show BrowserAction where
  show (FocusTab t) = "FocusTab " <> show t
  show (CreateTab w u) = "CreateTab " <> show w <> " " <> show u
  show (CreateWindow us) = "CreateWindow " <> show us
  show (MoveTabToWindow t w i) = "MoveTabToWindow " <> show t <> " " <> show w <> " " <> show i
  show (NewWindowWithTabs ts) = "NewWindowWithTabs " <> show ts
  show (RemoveTab t) = "RemoveTab " <> show t

-- | Where a restored tab should reopen, decided by its IMMEDIATE PARENT — the
-- | container that owns it (a node's owning window is its immediate parent).
data RestoreTarget
  = IntoWindow Int -- parent already live as a window (reopen the tab back into it)
  | IntoNewWindow NodeId -- saved-container parent (its tabs open one new window it goes live as)
  | IntoCurrent -- no parent container (reopen in the current window)

derive instance eqRestoreTarget :: Eq RestoreTarget

type CmdResult = { model :: Model, patch :: Patch, actions :: Array BrowserAction }

applyCommand :: Number -> Command -> Model -> CmdResult
applyCommand now cmd model = case cmd of
  Collapse nid value -> withNode nid \n -> upsertOnly (n { collapsed = value })

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
    let tabIds = liveTabIds nid
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
          { model: applyPatch patch model, patch, actions: map RemoveTab (liveTabIds nid) }

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

  NewGroup mParent index ->
    let
      -- fall back to top-level if the named parent doesn't exist
      effParent = case mParent of
        Just pid | Map.member pid model.nodes -> mParent
        _ -> Nothing
      nid = "n" <> show model.nextId
      g = (defaultNode nid KGroup now) { title = "New group", parent = effParent }
      patch =
        { upserts: [ g ] <> insertUpserts effParent index nid
        , removes: []
        , roots: rootInsert effParent index nid
        }
      model' = (applyPatch patch model) { nextId = model.nextId + 1 }
    in
      { model: model', patch, actions: [] }

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

  -- After an edit detached a child from `mParent`, prune that parent if it is now a
  -- childless, un-renamed group (cascading up), folding the removal into the edit's
  -- patch. (A live-tab move detaches later, via the browser event, so it prunes in
  -- Model.Reconcile instead.)
  withPrune :: Maybe NodeId -> CmdResult -> CmdResult
  withPrune mParent r = case mParent of
    Nothing -> r
    Just pid -> let p = pruneFrom pid r.model in { model: p.model, patch: mergePatch r.patch p.patch, actions: r.actions }

  -- live tab ids in the subtree rooted at nid (a tabId is present only on a live tab)
  liveTabIds :: NodeId -> Array Int
  liveTabIds nid = Array.mapMaybe
    (\i -> Map.lookup i model.nodes >>= _.tabId)
    (subtreeIds nid model)

  -- upsert that removes nid from whatever parent currently holds it (window/group)
  detachUpserts :: Node -> Array Node
  detachUpserts node = case node.parent of
    Just pid -> case Map.lookup pid model.nodes of
      Just p -> [ p { children = Array.delete node.id p.children } ]
      Nothing -> []
    Nothing -> []

  -- restore: re-open every closed tab in the subtree, re-binding to existing
  -- nodes via pendingRestore (keyed by the window each tab is recreated in) when
  -- each onCreated arrives. Each tab is routed by its immediate parent: a parent
  -- already live as a window reopens the tab back into it; a saved-group parent
  -- has all its tabs grouped into one new browser window (whose node rebinds via
  -- pendingRestoreWindows) and goes live in place; a tab with no parent reopens in
  -- the current window.
  restore :: NodeId -> CmdResult
  restore nid =
    let
      closedTabs = Array.filter (\n -> n.kind == KTab && not (isLiveTab n))
        (Array.mapMaybe (\i -> Map.lookup i model.nodes) (subtreeIds nid model))
      -- only tabs with a url can be reopened; keep subtree (preorder) order
      tagged = Array.mapMaybe
        (\n -> map (\u -> { id: n.id, url: u, target: restoreTargetOf model n.id }) n.url)
        closedTabs

      -- one new window per closed-window ancestor, in first-seen order
      newWinIds = Array.nub (Array.mapMaybe (\x -> case x.target of
        IntoNewWindow w -> Just w
        _ -> Nothing) tagged)
      forWindow w = Array.filter (\x -> x.target == IntoNewWindow w) tagged
      windowActions = map (\w -> CreateWindow (map _.url (forWindow w))) newWinIds
      -- carry the EXACT node ids (same order as the urls above) so each rebinds to
      -- the right node when the window's tabs arrive — not "all of the container's
      -- closed children", which a partial restore must not resurrect.
      newWindows = map (\w -> { node: w, tabs: List.fromFoldable (map _.id (forWindow w)) }) newWinIds

      tabActions = Array.mapMaybe (\x -> case x.target of
        IntoWindow wid -> Just (CreateTab (Just wid) (Just x.url))
        IntoCurrent -> Just (CreateTab Nothing (Just x.url))
        IntoNewWindow _ -> Nothing) tagged

      -- queue each IntoWindow tab under its target window — a FIFO consumed as the
      -- recreated tabs' onCreated events arrive (in this same order). IntoNewWindow
      -- tabs are queued when their window opens (Model.Reconcile); IntoCurrent tabs
      -- can't be pre-keyed by a window, so they just reopen as fresh nodes.
      queueIntoWindow m x = case x.target of
        IntoWindow wid -> Map.alter (\ml -> Just (maybe (List.singleton x.id) (\l -> List.snoc l x.id) ml)) wid m
        _ -> m
      pending' = foldl queueIntoWindow model.pendingRestore tagged

      -- Mark every closed tab we are reopening: if the *browser* later closes it,
      -- the restored copy is dropped instead of re-saved (Reconcile.TabClosed). The
      -- flag is set HERE — where we know a genuine user restore is happening — and
      -- not in `rebindRestored`, because a live tab rehomed into a saved group also
      -- rebinds via `pendingRestore`; flagging at the rebind would mistake that
      -- (and any later tab in that window) for a restore and wrongly drop it.
      flagged = Array.mapMaybe
        (\x -> (\n -> n { restoredFromClosed = true }) <$> Map.lookup x.id model.nodes) tagged
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

  -- insert child id into mParent's children at index (returns the parent upsert)
  insertUpserts :: Maybe NodeId -> Int -> NodeId -> Array Node
  insertUpserts mParent index child = case mParent of
    Just pid -> case Map.lookup pid model.nodes of
      Just p -> [ p { children = insertAtClamped index child p.children } ]
      Nothing -> []
    Nothing -> []

  rootInsert :: Maybe NodeId -> Int -> NodeId -> Maybe (Array NodeId)
  rootInsert mParent index child = case mParent of
    Nothing -> Just (insertAtClamped index child model.roots)
    Just _ -> Nothing

  move :: NodeId -> Maybe NodeId -> Int -> CmdResult
  move nid mParent index = case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node
      -- reject a move into the node's own subtree (O(depth) upward walk)
      | mParent == Just nid || maybe false (\p -> isAncestorOrSelf nid p model) mParent -> noChange
      -- a live tab changing its owning window (its immediate parent): drive the
      -- real browser tab instead of editing the tree; the tree re-settles from the
      -- resulting onAttached/onCreated events, so this emits no patch.
      | isLiveTab node && mParent /= node.parent -> moveLiveTab node mParent index
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
    Just t -> case mParent >>= (\pid -> Map.lookup pid model.nodes) >>= _.windowId of
      -- into an already-live window: move the tab there at the dropped position
      Just w -> actionsOnly [ MoveTabToWindow t w index ]
      -- new-window cases (a plain container goes live, or out to the root)
      Nothing -> let r = rehome model mParent [ t ] in { model: r.model, patch: emptyPatch, actions: r.actions }

  -- Browser action(s) to re-home live `tabIds` to container `mParent` (their new
  -- owning window): into an already-live window -> move each there; into a
  -- not-yet-live container -> queue it to bind one new window holding them all
  -- (it "goes live", rebinding on onCreated like a restore); to the root -> a
  -- fresh window holding them all.
  rehome :: Model -> Maybe NodeId -> Array Int -> { model :: Model, actions :: Array BrowserAction }
  rehome m mParent tabIds
    | Array.null tabIds = { model: m, actions: [] }
    | otherwise = case mParent of
        Nothing -> { model: m, actions: [ NewWindowWithTabs tabIds ] }
        Just pid -> case Map.lookup pid m.nodes of
          Just p | Just w <- p.windowId -> { model: m, actions: map (\t -> MoveTabToWindow t w (-1)) tabIds }
          -- de-dupe the queue so two drags into the same not-yet-live container
          -- can't both pop a window and double-bind it
          Just _ -> { model: m { pendingRestoreWindows = pushPending pid m.pendingRestoreWindows }, actions: [ NewWindowWithTabs tabIds ] }
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
            withBrowser patch =
              let br = rehome (applyPatch patch model) node.parent kidTabIds
              in { model: br.model, patch, actions: br.actions }
          in
            case node.parent of
              Just pid -> case Map.lookup pid model.nodes of
                Nothing -> noChange
                Just p -> withPrune node.parent (withBrowser { upserts: [ p { children = spliceReplace nid kids p.children } ] <> promote (Just pid), removes: [ nid ], roots: Nothing })
              Nothing -> withBrowser { upserts: promote Nothing, removes: [ nid ], roots: Just (spliceReplace nid kids model.roots) }

spliceReplace :: NodeId -> Array NodeId -> Array NodeId -> Array NodeId
spliceReplace x ys = Array.concatMap (\e -> if e == x then ys else [ e ])

-- Append a container to the pending-window queue unless it is already waiting. A
-- rehome carries no tabs to rebind (the dragged tab arrives via onAttached); the
-- container just needs to bind the new window.
pushPending :: NodeId -> Array PendingWindow -> Array PendingWindow
pushPending pid xs = if Array.any (\e -> e.node == pid) xs then xs else Array.snoc xs { node: pid, tabs: Nil }

-- | Where a closed tab node should reopen, decided by its IMMEDIATE parent — the
-- | container that owns it (a node's owning window is its immediate parent). A
-- | parent already live as a window -> back into that window; a parent that is a
-- | saved group -> a new window that the group goes live as; no parent (a bare
-- | root tab) -> the current window.
restoreTargetOf :: Model -> NodeId -> RestoreTarget
restoreTargetOf model nid = case parentNode nid of
  Just p
    | Just wid <- p.windowId -> IntoWindow wid
    | otherwise -> IntoNewWindow p.id
  Nothing -> IntoCurrent
  where
  parentNode id = (Map.lookup id model.nodes >>= _.parent) >>= \pid -> Map.lookup pid model.nodes

-- Request protocol -----------------------------------------------------------

-- A window of the visible order: rows [start, start+count) of the order for
-- `query`, with the active tab's index in `myWindow` when `wantFocus`. With
-- `tail`, `start` is ignored and the *last* window is returned (the open default,
-- since new windows land at the bottom — that's where the live nodes are).
type ViewReq = { start :: Int, count :: Int, query :: String, myWindow :: Maybe Int, wantFocus :: Boolean, tail :: Boolean }

data Request = GetView ViewReq | RunCommand Command | Undo | Redo | Export

encodeRequest :: Request -> Json
encodeRequest (GetView r) = encodeJson
  { tag: "getView", start: r.start, count: r.count, query: r.query, myWindow: r.myWindow, wantFocus: r.wantFocus, tail: r.tail }
encodeRequest (RunCommand c) = encodeJson { tag: "command", body: encodeCommand c }
encodeRequest Undo = encodeJson { tag: "undo" }
encodeRequest Redo = encodeJson { tag: "redo" }
encodeRequest Export = encodeJson { tag: "export" }

decodeRequest :: Json -> Either String Request
decodeRequest json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "getView" -> GetView <$> (dec json :: Either String ViewReq)
    "command" -> do
      { body } <- dec json :: Either String { body :: Json }
      RunCommand <$> decodeCommand body
    "undo" -> Right Undo
    "redo" -> Right Redo
    "export" -> Right Export
    other -> Left ("unknown request: " <> other)

encodeCommand :: Command -> Json
encodeCommand = case _ of
  Collapse nid value -> encodeJson { tag: "collapse", id: nid, value }
  Rename nid title -> encodeJson { tag: "rename", id: nid, title }
  Activate nid -> encodeJson { tag: "activate", id: nid }
  CloseNode nid -> encodeJson { tag: "close", id: nid }
  Delete nid -> encodeJson { tag: "delete", id: nid }
  Move nid parent index -> encodeJson { tag: "move", id: nid, parent, index }
  MoveTopLevel nid -> encodeJson { tag: "moveTopLevel", id: nid }
  MoveBottom nid -> encodeJson { tag: "moveBottom", id: nid }
  Flatten nid -> encodeJson { tag: "flatten", id: nid }
  NewGroup parent index -> encodeJson { tag: "newGroup", parent, index }
  Import snap -> encodeJson { tag: "import", body: encodeSnapshotData snap }
  Drop drag target -> encodeJson { tag: "drop", drag, target }

decodeCommand :: Json -> Either String Command
decodeCommand json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "collapse" -> (\r -> Collapse r.id r.value) <$> (dec json :: Either String { id :: NodeId, value :: Boolean })
    "rename" -> (\r -> Rename r.id r.title) <$> (dec json :: Either String { id :: NodeId, title :: String })
    "activate" -> (\r -> Activate r.id) <$> (dec json :: Either String { id :: NodeId })
    "close" -> (\r -> CloseNode r.id) <$> (dec json :: Either String { id :: NodeId })
    "delete" -> (\r -> Delete r.id) <$> (dec json :: Either String { id :: NodeId })
    "move" -> (\r -> Move r.id r.parent r.index) <$> (dec json :: Either String { id :: NodeId, parent :: Maybe NodeId, index :: Int })
    "moveTopLevel" -> (\r -> MoveTopLevel r.id) <$> (dec json :: Either String { id :: NodeId })
    "moveBottom" -> (\r -> MoveBottom r.id) <$> (dec json :: Either String { id :: NodeId })
    "flatten" -> (\r -> Flatten r.id) <$> (dec json :: Either String { id :: NodeId })
    "newGroup" -> (\r -> NewGroup r.parent r.index) <$> (dec json :: Either String { parent :: Maybe NodeId, index :: Int })
    "import" -> do
      { body } <- dec json :: Either String { body :: Json }
      Import <$> decodeSnapshot body
    "drop" -> (\r -> Drop r.drag r.target) <$> (dec json :: Either String { drag :: NodeId, target :: NodeId })
    other -> Left ("unknown command: " <> other)

dec :: forall a. DecodeJson a => Json -> Either String a
dec = lmap printJsonDecodeError <<< decodeJson
