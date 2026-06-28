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
  , removed :: Set NodeId -- prior-live tabs dropped (fresh, orphaned in a reopened window)
  , windowsGainedTab :: Set NodeId -- window nodes that gained a tab moved in from another window — too ambiguous to drop an orphan from
  , anyFreshTab :: Boolean -- a brand-new (unmatched) tab appeared anywhere: it could be ANY orphan reopened under a changed url, so suppress all drops this run
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

    -- Window nodes whose match is too ambiguous to safely DROP an orphan from. A url
    -- shared across more than one prior window can bind the wrong window node at
    -- re-match (url matching is approximate), so a tab is never dropped from such a
    -- window — only from one whose urls are all unique. Dropping is conservative on
    -- purpose: when unsure, keep (the prior behaviour), never delete.
    urlWins = foldl addUrlWin Map.empty priorTabs
    addUrlWin m n = case n.url, n.parent of
      Just u, Just p -> Map.insertWith Set.union u (Set.singleton p) m
      _, _ -> m
    sharedUrl u = maybe false (\s -> Set.size s >= 2) (Map.lookup u urlWins)
    windowsWithSharedUrl = foldl
      (\acc n -> case n.url, n.parent of
        Just u, Just p | sharedUrl u -> Set.insert p acc
        _, _ -> acc)
      Set.empty
      priorTabs

    acc0 =
      { nodes: model0.nodes
      , roots: model0.roots
      , byTab: Map.empty
      , byWindow: Map.empty
      , pool: pool0
      , consumedTabs: Set.empty
      , consumedWindows: Set.empty
      , touched: Set.empty
      , removed: Set.empty
      , windowsGainedTab: Set.empty
      , anyFreshTab: false
      , nextId: model0.nextId
      }
    acc1 = foldl (processWindow now urlToWin) acc0 current
    -- prior-live tabs that did not reopen. A fresh (never-restored) tab orphaned in
    -- a window that DID reopen is dropped — closing the tab rule's gap when the event
    -- page was suspended (it matches the original, which deletes such orphans). A
    -- restored tab is kept (it belongs in the tree); and a tab whose whole window did
    -- not reopen is kept too, preserving that window as a recoverable previous session.
    -- Drop a prior-live tab only when we are SURE it was a fresh tab orphaned in a
    -- window that genuinely reopened: never-restored, no unmatched/fresh tab appeared
    -- anywhere this run (a changed-url reopen would land as one, and could be any
    -- orphan), its window reopened, that window has unambiguous (unique) urls, and it
    -- gained no tab moved in from another window. Anything less → keep, in place.
    -- WAIVED (inherent to url matching, like the module header's edge stance): a tab
    -- that BOTH moves to another window AND changes its url to exactly match a tab
    -- that was closed there is indistinguishable from a real close, so the orphan it
    -- leaves behind is dropped. A multi-coincidence, on par with two same-url tabs in
    -- one window; ruling it out would mean never dropping (the url is all we have).
    dropsClean a n =
      not n.restoredFromClosed
        && not a.anyFreshTab
        && windowReopened a n
        && maybe false (\p -> not (Set.member p windowsWithSharedUrl)) n.parent
        && maybe false (\p -> not (Set.member p a.windowsGainedTab)) n.parent
    acc2 = foldl
      ( \a n ->
          if Set.member n.id a.consumedTabs then a
          else if dropsClean a n then removeInAcc a n.id
          else closeInAcc now a n.id
      )
      acc1
      priorTabs
    -- close prior-live windows that did not reopen
    acc3 = foldl (\a n -> if Set.member n.id a.consumedWindows then a else closeInAcc now a n.id) acc2 priorWindows

    model' = model0
      { nodes = acc3.nodes
      , roots = acc3.roots
      , byTab = acc3.byTab
      , byWindow = acc3.byWindow
      , nextId = acc3.nextId
      }
    upserts = Array.mapMaybe
      (\i -> if Set.member i acc3.removed then Nothing else Map.lookup i acc3.nodes)
      (Array.fromFoldable (Set.toUnfoldable acc3.touched :: List NodeId))
    removes = Array.fromFoldable (Set.toUnfoldable acc3.removed :: List NodeId)
    roots = if acc3.roots == model0.roots then Nothing else Just acc3.roots
  in
    { model: model', patch: { upserts, removes, roots } }

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

-- Re-bind a reopened tab to its existing node, or create a new one. A tab matched
-- to a node already under the chosen window stays in place (preserving the user's
-- organization); a tab matched to a node from ANOTHER (now-closing) prior window is
-- moved into this window — otherwise one live window's tabs would stay scattered
-- across the separate prior windows they happened to come from.
matchTab :: Number -> NodeId -> Acc -> RuntimeTab -> Acc
matchTab now winId acc ct = case ct.url >>= \u -> popPoolFor winId u acc of
  Just (Tuple nid pool') -> case Map.lookup nid acc.nodes of
    Just n
      | n.parent == Just winId -> consume nid (Map.insert nid (rebind n) acc.nodes) acc.roots pool' acc.touched
      | otherwise ->
          let
            nodes1 = Map.insert nid ((rebind n) { parent = Just winId }) acc.nodes
            -- detach from its old parent's child list (or the roots)
            nodes2 = case n.parent >>= (\pid -> Map.lookup pid nodes1) of
              Just p -> Map.insert p.id (p { children = Array.delete nid p.children }) nodes1
              Nothing -> nodes1
            -- attach into the chosen window at the browser position
            nodes3 = case Map.lookup winId nodes2 of
              Just w -> Map.insert winId (w { children = insertAtClamped ct.index nid (Array.delete nid w.children) }) nodes2
              Nothing -> nodes2
            touched' = Set.insert winId (maybe acc.touched (\pid -> Set.insert pid acc.touched) n.parent)
          in
            -- this window gained a tab moved in from another window: mark it ambiguous
            -- so an orphan here is not dropped (the moved-in tab might be that orphan
            -- reopened under a changed url that happened to match the other window's tab)
            (consume nid nodes3 (Array.delete nid acc.roots) pool' touched')
              { windowsGainedTab = Set.insert winId acc.windowsGainedTab }
    Nothing -> freshTab now winId acc ct
  Nothing -> freshTab now winId acc ct
  where
  consume nid nodes roots pool' touched = acc
    { nodes = nodes
    , roots = roots
    , byTab = Map.insert ct.tabId nid acc.byTab
    , pool = pool'
    , consumedTabs = Set.insert nid acc.consumedTabs
    , touched = Set.insert nid touched
    }
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
      , anyFreshTab = true
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
    { nodes = Map.insert nid (n { tabId = Nothing, windowId = Nothing, active = false, closedAt = Just now, restoredFromClosed = false }) acc.nodes
    , touched = Set.insert nid acc.touched
    }
  Nothing -> acc

-- Did this tab's owning window reopen (its window node bound to a current browser
-- window)? If so, the tab is orphaned in a still-open window; if not, its whole
-- window is gone and the tab is preserved with it.
windowReopened :: Acc -> Node -> Boolean
windowReopened acc n = case n.parent of
  Just pid -> Set.member pid acc.consumedWindows
  Nothing -> false

-- Drop a prior-live tab node entirely (a fresh, never-restored tab that did not
-- reopen): unlink it from its window's child list and the node map, and record the
-- removal so the patch deletes the persisted record (else it would reload next boot).
removeInAcc :: Acc -> NodeId -> Acc
removeInAcc acc nid = case Map.lookup nid acc.nodes of
  Just n ->
    let
      nodes1 = Map.delete nid acc.nodes
      nodes2 = case n.parent >>= (\pid -> Map.lookup pid nodes1) of
        Just p -> Map.insert p.id (p { children = Array.delete nid p.children }) nodes1
        Nothing -> nodes1
    in
      acc
        { nodes = nodes2
        , roots = Array.delete nid acc.roots
        , removed = Set.insert nid acc.removed
        , touched = maybe acc.touched (\pid -> Set.insert pid acc.touched) n.parent
        }
  Nothing -> acc
