-- | Startup re-match: the one bounded heuristic the design opts into. After a
-- | restart the persisted forest still holds last session's "live" nodes, but
-- | their browser ids are stale. We re-bind each currently-open tab to its
-- | existing node BY URL, leaving the node exactly where the user organized it
-- | (its tree position, custom title, and collapse survive the restart). Tabs
-- | that didn't reopen close in place; windows that didn't reopen close as a
-- | restorable "previous session"; genuinely new tabs/windows are created.
-- |
-- | A clean restart (same windows/tabs) therefore reproduces the prior tree
-- | exactly, just with fresh ids — no duplication. Cost is O(live tabs), and the
-- | returned patch touches only changed nodes (so the boot write stays O(change)
-- | after the first run). This is intentionally approximate at the edges.
module Model.Rematch (rematchOnStartup) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Model.Tree (insertAtClamped)
import Model.Types (Kind(..), Model, Node, NodeId, RuntimeTab, RuntimeWindow, defaultNode, isLiveTab)

type Acc =
  { nodes :: Map NodeId Node
  , roots :: Array NodeId
  , byTab :: Map Int NodeId
  , byWindow :: Map Int NodeId
  , pool :: Map String (List NodeId) -- prior-live tab nodes by url, to consume
  , consumedTabs :: Set NodeId
  , consumedWindows :: Set NodeId
  , touched :: Set NodeId
  , nextId :: Int
  }

-- | Returns the re-matched model and a patch of only the changed nodes.
rematchOnStartup :: Number -> Array RuntimeWindow -> Model -> { model :: Model, patch :: { upserts :: Array Node, removes :: Array NodeId, roots :: Maybe (Array NodeId) } }
rematchOnStartup now current model0 =
  let
    allN = Array.fromFoldable (Map.values model0.nodes)
    priorTabs = Array.filter (\n -> n.kind == KTab && isLiveTab n) allN
    priorWindows = Array.filter (\n -> n.kind == KGroup && isJust n.windowId) allN
    pool0 = foldl addToPool Map.empty priorTabs
    urlToWin = foldl addUrlWindow Map.empty priorTabs

    acc0 =
      { nodes: model0.nodes
      , roots: model0.roots
      , byTab: Map.empty
      , byWindow: Map.empty
      , pool: pool0
      , consumedTabs: Set.empty
      , consumedWindows: Set.empty
      , touched: Set.empty
      , nextId: model0.nextId
      }
    acc1 = foldl (processWindow now urlToWin) acc0 current
    -- close prior-live tabs that did not reopen, in place
    acc2 = foldl (\a n -> if Set.member n.id a.consumedTabs then a else closeInAcc now a n.id) acc1 priorTabs
    -- close prior-live windows that did not reopen
    acc3 = foldl (\a n -> if Set.member n.id a.consumedWindows then a else closeInAcc now a n.id) acc2 priorWindows

    model' = model0
      { nodes = acc3.nodes
      , roots = acc3.roots
      , byTab = acc3.byTab
      , byWindow = acc3.byWindow
      , nextId = acc3.nextId
      }
    upserts = Array.mapMaybe (\i -> Map.lookup i acc3.nodes) (Array.fromFoldable (Set.toUnfoldable acc3.touched :: List NodeId))
    roots = if acc3.roots == model0.roots then Nothing else Just acc3.roots
  in
    { model: model', patch: { upserts, removes: [], roots } }

addToPool :: Map String (List NodeId) -> Node -> Map String (List NodeId)
addToPool m n = case n.url of
  Just u -> Map.alter (Just <<< maybe (List.singleton n.id) (Cons n.id)) u m
  Nothing -> m

-- url -> the container that owns a prior-live tab with that url (its immediate
-- parent — a node's owning window is its immediate parent), for matching a
-- reopened browser window to the container it should re-bind.
addUrlWindow :: Map String NodeId -> Node -> Map String NodeId
addUrlWindow m n = case n.url, n.parent of
  Just u, Just p -> Map.insert u p m
  _, _ -> m

mkId :: Int -> NodeId
mkId i = "n" <> show i

processWindow :: Number -> Map String NodeId -> Acc -> RuntimeWindow -> Acc
processWindow now urlToWin acc cw =
  let Tuple winId acc1 = resolveWindow now urlToWin acc cw
  in foldl (matchTab now winId) acc1 cw.tabs

-- Reuse the best-matching prior window (most shared urls) or create a fresh one.
resolveWindow :: Number -> Map String NodeId -> Acc -> RuntimeWindow -> Tuple NodeId Acc
resolveWindow now urlToWin acc cw = case chooseWindow urlToWin acc cw of
  Just wid -> case Map.lookup wid acc.nodes of
    Just w ->
      Tuple wid acc
        { nodes = Map.insert wid (w { windowId = Just cw.windowId, closedAt = Nothing }) acc.nodes
        , byWindow = Map.insert cw.windowId wid acc.byWindow
        , consumedWindows = Set.insert wid acc.consumedWindows
        , touched = Set.insert wid acc.touched
        }
    Nothing -> freshWindow now acc cw
  Nothing -> freshWindow now acc cw

freshWindow :: Number -> Acc -> RuntimeWindow -> Tuple NodeId Acc
freshWindow now acc cw =
  let
    nid = mkId acc.nextId
    w = (defaultNode nid KGroup now) { windowId = Just cw.windowId, title = "Window" }
  in
    Tuple nid acc
      { nodes = Map.insert nid w acc.nodes
      , roots = Array.snoc acc.roots nid
      , byWindow = Map.insert cw.windowId nid acc.byWindow
      , touched = Set.insert nid acc.touched
      , nextId = acc.nextId + 1
      }

chooseWindow :: Map String NodeId -> Acc -> RuntimeWindow -> Maybe NodeId
chooseWindow urlToWin acc cw =
  let
    tally = foldl bump Map.empty cw.tabs
    bump m ct = case ct.url >>= \u -> Map.lookup u urlToWin of
      Just wid | not (Set.member wid acc.consumedWindows), Map.member wid acc.nodes ->
        Map.insertWith (+) wid 1 m
      _ -> m
    ranked = Array.sortBy (\a b -> compare (snd b) (snd a)) (Map.toUnfoldable tally :: Array (Tuple NodeId Int))
  in
    map fst (Array.head ranked)

-- Re-bind a reopened tab to its existing node IN PLACE, or create a new node.
matchTab :: Number -> NodeId -> Acc -> RuntimeTab -> Acc
matchTab now winId acc ct = case ct.url >>= \u -> popPoolFor winId u acc of
  Just (Tuple nid pool') -> case Map.lookup nid acc.nodes of
    Just n -> acc
      { nodes = Map.insert nid (rebind n) acc.nodes
      , byTab = Map.insert ct.tabId nid acc.byTab
      , pool = pool'
      , consumedTabs = Set.insert nid acc.consumedTabs
      , touched = Set.insert nid acc.touched
      }
    Nothing -> freshTab now winId acc ct
  Nothing -> freshTab now winId acc ct
  where
  rebind n = n
    { tabId = Just ct.tabId
    , title = ct.title
    , url = ct.url
    , favIconUrl = ct.favIconUrl
    , active = ct.active
    , closedAt = Nothing
    }

freshTab :: Number -> NodeId -> Acc -> RuntimeTab -> Acc
freshTab now winId acc ct =
  let
    nid = mkId acc.nextId
    n = (defaultNode nid KTab now)
      { title = ct.title, url = ct.url, favIconUrl = ct.favIconUrl, active = ct.active, tabId = Just ct.tabId, parent = Just winId }
    win = Map.lookup winId acc.nodes
    nodes' = case win of
      Just w -> Map.insert winId (w { children = insertAtClamped ct.index nid w.children }) (Map.insert nid n acc.nodes)
      Nothing -> Map.insert nid n acc.nodes
  in
    acc
      { nodes = nodes'
      , byTab = Map.insert ct.tabId nid acc.byTab
      , touched = Set.insert nid (Set.insert winId acc.touched)
      , nextId = acc.nextId + 1
      }

-- Pop a prior-live tab node bound to url `u`, PREFERRING one that was a child of
-- the chosen window (so duplicate urls across windows don't steal each other's
-- nodes), falling back to any matching node.
popPoolFor :: NodeId -> String -> Acc -> Maybe (Tuple NodeId (Map String (List NodeId)))
popPoolFor winId u acc = do
  lst <- Map.lookup u acc.pool
  let ownedByWindow nid = (Map.lookup nid acc.nodes >>= _.parent) == Just winId
  case List.find ownedByWindow lst of
    Just nid -> pure (Tuple nid (Map.insert u (List.delete nid lst) acc.pool))
    Nothing -> do
      { head, tail } <- List.uncons lst
      pure (Tuple head (Map.insert u tail acc.pool))

closeInAcc :: Number -> Acc -> NodeId -> Acc
closeInAcc now acc nid = case Map.lookup nid acc.nodes of
  Just n -> acc
    { nodes = Map.insert nid (n { tabId = Nothing, windowId = Nothing, active = false, closedAt = Just now }) acc.nodes
    , touched = Set.insert nid acc.touched
    }
  Nothing -> acc
