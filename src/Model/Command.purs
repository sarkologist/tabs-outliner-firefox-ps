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
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Model.Tree (applyPatch, insertAtClamped, isAncestorOrSelf, subtreeIds)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, Status(..), defaultNode, emptyPatch)

data Command
  = Collapse NodeId Boolean
  | Rename NodeId String
  | Activate NodeId -- focus a live tab, or restore a closed one
  | CloseNode NodeId -- close the live tabs in the subtree (keep history)
  | Delete NodeId -- remove the subtree from the tree (and close its live tabs)
  | Move NodeId (Maybe NodeId) Int -- node, new parent (Nothing = root), index
  | Flatten NodeId -- dissolve a group, promoting its children
  | NewGroup (Maybe NodeId) Int -- new folder under parent at index

-- | Browser-side effects a command implies (interpreted by the background).
data BrowserAction
  = FocusTab Int
  | CreateTab (Maybe Int) (Maybe String)
  | RemoveTab Int

derive instance eqBrowserAction :: Eq BrowserAction
instance showBrowserAction :: Show BrowserAction where
  show (FocusTab t) = "FocusTab " <> show t
  show (CreateTab w u) = "CreateTab " <> show w <> " " <> show u
  show (RemoveTab t) = "RemoveTab " <> show t

type CmdResult = { model :: Model, patch :: Patch, actions :: Array BrowserAction }

applyCommand :: Number -> Command -> Model -> CmdResult
applyCommand now cmd model = case cmd of
  Collapse nid value -> withNode nid \n -> upsertOnly (n { collapsed = value })

  Rename nid title -> withNode nid \n -> upsertOnly (n { customTitle = Just title })

  Activate nid -> withNode nid \n -> case n.status, n.tabId of
    Live, Just t -> actionsOnly [ FocusTab t ]
    _, _ -> restore nid

  CloseNode nid -> actionsOnly (map RemoveTab (liveTabIds nid))

  Delete nid -> case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node ->
      let
        ids = subtreeIds nid model
        parentUpserts = detachUpserts node
        rootsM = if Array.elem nid model.roots then Just (Array.delete nid model.roots) else Nothing
        patch = { upserts: parentUpserts, removes: ids, roots: rootsM }
      in
        { model: applyPatch patch model, patch, actions: map RemoveTab (liveTabIds nid) }

  Move nid mParent index -> move nid mParent index

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

  -- live tab ids in the subtree rooted at nid
  liveTabIds :: NodeId -> Array Int
  liveTabIds nid = Array.mapMaybe
    (\i -> Map.lookup i model.nodes >>= \n -> if n.status == Live then n.tabId else Nothing)
    (subtreeIds nid model)

  -- upsert that removes nid from whatever parent currently holds it (window/group)
  detachUpserts :: Node -> Array Node
  detachUpserts node = case node.parent of
    Just pid -> case Map.lookup pid model.nodes of
      Just p -> [ p { children = Array.delete node.id p.children } ]
      Nothing -> []
    Nothing -> []

  -- restore: re-open every closed tab in the subtree, re-binding to existing
  -- nodes via pendingRestore (keyed by url) when each onCreated arrives.
  restore :: NodeId -> CmdResult
  restore nid =
    let
      closedTabs = Array.filter (\n -> n.status == Closed && n.kind == KTab)
        (Array.mapMaybe (\i -> Map.lookup i model.nodes) (subtreeIds nid model))
      withUrl = Array.mapMaybe (\n -> map (\u -> Tuple u n.id) n.url) closedTabs
      pending' = foldl (\m (Tuple u i) -> Map.insert u i m) model.pendingRestore withUrl
      actions = map (\(Tuple u _) -> CreateTab Nothing (Just u)) withUrl
    in
      { model: model { pendingRestore = pending' }, patch: emptyPatch, actions }

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
    Just node ->
      -- reject a move into the node's own subtree (O(depth) upward walk)
      if mParent == Just nid || maybe false (\p -> isAncestorOrSelf nid p model) mParent then noChange
      else
          let
            detached = detachUpserts node
            rootsDetached = Array.delete nid model.roots
            node' = node { parent = mParent }
          in
            case mParent of
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

  flatten :: NodeId -> CmdResult
  flatten nid = case Map.lookup nid model.nodes of
    Nothing -> noChange
    Just node
      | node.kind /= KGroup -> noChange -- only dissolve user groups, not live windows/tabs
      | otherwise ->
          let
            kids = node.children
            promote parentRef = Array.mapMaybe
              (\cid -> (\c -> c { parent = parentRef }) <$> Map.lookup cid model.nodes)
              kids
          in
            case node.parent of
              Just pid -> case Map.lookup pid model.nodes of
                Nothing -> noChange
                Just p ->
                  let
                    p' = p { children = spliceReplace nid kids p.children }
                    patch = { upserts: [ p' ] <> promote (Just pid), removes: [ nid ], roots: Nothing }
                  in
                    { model: applyPatch patch model, patch, actions: [] }
              Nothing ->
                let patch = { upserts: promote Nothing, removes: [ nid ], roots: Just (spliceReplace nid kids model.roots) }
                in { model: applyPatch patch model, patch, actions: [] }

spliceReplace :: NodeId -> Array NodeId -> Array NodeId -> Array NodeId
spliceReplace x ys = Array.concatMap (\e -> if e == x then ys else [ e ])

-- Request protocol -----------------------------------------------------------

data Request = GetSnapshot | RunCommand Command

encodeRequest :: Request -> Json
encodeRequest GetSnapshot = encodeJson { tag: "getSnapshot" }
encodeRequest (RunCommand c) = encodeJson { tag: "command", body: encodeCommand c }

decodeRequest :: Json -> Either String Request
decodeRequest json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "getSnapshot" -> Right GetSnapshot
    "command" -> do
      { body } <- dec json :: Either String { body :: Json }
      RunCommand <$> decodeCommand body
    other -> Left ("unknown request: " <> other)

encodeCommand :: Command -> Json
encodeCommand = case _ of
  Collapse nid value -> encodeJson { tag: "collapse", id: nid, value }
  Rename nid title -> encodeJson { tag: "rename", id: nid, title }
  Activate nid -> encodeJson { tag: "activate", id: nid }
  CloseNode nid -> encodeJson { tag: "close", id: nid }
  Delete nid -> encodeJson { tag: "delete", id: nid }
  Move nid parent index -> encodeJson { tag: "move", id: nid, parent, index }
  Flatten nid -> encodeJson { tag: "flatten", id: nid }
  NewGroup parent index -> encodeJson { tag: "newGroup", parent, index }

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
    "flatten" -> (\r -> Flatten r.id) <$> (dec json :: Either String { id :: NodeId })
    "newGroup" -> (\r -> NewGroup r.parent r.index) <$> (dec json :: Either String { parent :: Maybe NodeId, index :: Int })
    other -> Left ("unknown command: " <> other)

dec :: forall a. DecodeJson a => Json -> Either String a
dec = lmap printJsonDecodeError <<< decodeJson
