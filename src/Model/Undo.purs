-- | Undo/redo as inverse patches. Every model-mutating user command already
-- | produces a `Patch`; the inverse of that patch — computed against the model as
-- | it was *before* the command — is the patch that reverts it. The background
-- | keeps a stack of these inverses, so undo/redo cost O(change) and reuse the
-- | very same persist + broadcast path a command does: no snapshots, no second
-- | way to apply a change, the view and the authority stay consistent by
-- | construction.
-- |
-- | Because the stacks live across arbitrary live browser activity, applying an
-- | entry *reconciles* it against the current model (see `applyEntry`), so undo
-- | never reverts browser state or drops a node a live event added meanwhile.
-- | A few wrinkles it handles so undo stays faithful:
-- |   * A live tab/window the command *removed* (Delete) can't have its browser
-- |     object resurrected by undo, so it comes back as Closed history — the same
-- |     restorable state a close would have left. Groups have no browser object,
-- |     so they come back unchanged.
-- |   * Restoring a pre-existing node touches only the fields a user command can
-- |     change (customTitle, collapsed, parent, children); the node's live state
-- |     (status, tabId, url, …) is kept from the current model, so undoing a
-- |     rename of a since-closed tab doesn't resurrect it as Live.
-- |   * `roots`/`children` are restored but merged with siblings that appeared
-- |     since (a window/tab opened between the command and the undo) so undo
-- |     never drops an unrelated new node out of the forest.
-- |
-- | Waived (matching the reducer's stance on cross-window moves): if a live event
-- | *re-parents* a node — a tab dragged to another window — between a structural
-- | command and its undo, undoing can leave that node in two parents' child lists.
-- | Bulletproofing that needs a field-level inverse / detach-on-reparent, i.e. the
-- | machinery this rewrite deliberately omits; the case is rare and self-corrects
-- | on the next edit of either parent.
module Model.Undo
  ( inversePatch
  , applyEntry
  , undoable
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (BrowserAction(..), Command(..))
import Model.Tree (applyPatch, isLive)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, Status(..))

-- | The patch that reverts `p`, given the model as it was *before* `p` was
-- | applied. Nodes `p` overwrote are restored to their prior state; nodes `p`
-- | removed are restored too (a live tab/window downgraded to Closed); nodes `p`
-- | created are removed. `roots` is restored iff `p` touched it.
inversePatch :: Number -> Model -> Patch -> Patch
inversePatch now before p =
  { upserts: restoredOverwrites <> restoredRemovals
  , removes: created
  , roots: case p.roots of
      Just _ -> Just before.roots
      Nothing -> Nothing
  }
  where
  prior id = Map.lookup id before.nodes
  -- a node the patch overwrote that existed before -> restore its old state as-is
  restoredOverwrites = Array.mapMaybe (\n -> prior n.id) p.upserts
  -- a node the patch removed that existed before -> restore it, but a live
  -- tab/window comes back Closed (its browser object is gone now)
  restoredRemovals = Array.mapMaybe (\id -> downgradeBrowser now <$> prior id) p.removes
  -- a node the patch created (in upserts, absent before) -> the inverse removes it
  created = Array.filter (\id -> not (Map.member id before.nodes)) (map _.id p.upserts)

-- | A removed live tab/window can't be brought back live by undo (its browser
-- | object is gone), so it returns as Closed history. Groups have no browser
-- | object and return unchanged.
downgradeBrowser :: Number -> Node -> Node
downgradeBrowser now n = case n.kind, n.status of
  KGroup, _ -> n
  _, Closed -> n
  _, Live -> n
    { status = Closed
    , tabId = Nothing
    , windowId = Nothing
    , active = false
    , closedAt = Just now
    }

-- | Append the "concurrent additions" to a restored sibling list: ids in the
-- | current list that the entry doesn't `known`-touch (neither restores in place,
-- | removes, nor re-parents via an upsert) — i.e. a node a live browser event
-- | added since. Without this, undo/redo would drop e.g. a tab opened in a window
-- | after the command ran. Ids the entry *does* touch follow the entry's intent
-- | (so redoing a move still removes the moved node from its old parent).
mergeSiblings :: Array NodeId -> Array NodeId -> Array NodeId -> Array NodeId
mergeSiblings known snap cur =
  snap <> Array.filter (\c -> not (Array.elem c snap) && not (Array.elem c known)) cur

-- | Reconcile a restored node against its current state. A user command only ever
-- | changes a *pre-existing* node's customTitle/collapsed/parent/children — never
-- | its browser-owned fields, which only live events touch — so undo restores
-- | exactly those and keeps the current live state (status, tabId, url, …). That
-- | way undoing a rename of a since-closed tab won't resurrect it as Live, and a
-- | restored parent keeps any child opened in it meanwhile. A node that no longer
-- | exists (an entry re-adding a removed subtree) uses the snapshot as-is (already
-- | downgraded by `inversePatch`).
reconcileNode :: Array NodeId -> Model -> Node -> Node
reconcileNode known model snap = case Map.lookup snap.id model.nodes of
  Nothing -> snap
  Just cur -> cur
    { customTitle = snap.customTitle
    , collapsed = snap.collapsed
    , parent = snap.parent
    , children = mergeSiblings known snap.children cur.children
    }

-- | The live browser tab id of a node, if it is a live tab — what to RemoveTab.
liveTab :: Model -> NodeId -> Maybe Int
liveTab model id = Map.lookup id model.nodes >>= \n ->
  if isLive n && n.kind == KTab then n.tabId else Nothing

type Applied = { model :: Model, patch :: Patch, inverse :: Patch, actions :: Array BrowserAction }

-- | Apply a stored undo/redo entry to the *current* model, reconciling it against
-- | live state (see `reconcileNode`/`mergeSiblings`). Returns the patch actually
-- | applied (to persist + broadcast), its inverse (to push on the opposite stack),
-- | and the browser actions it implies — closing any live tab whose node it
-- | removes (e.g. redoing a delete after that tab was restored), so a real tab
-- | never outlives its node. Reuses `applyPatch`, exactly as a command does.
applyEntry :: Number -> Patch -> Model -> Applied
applyEntry now entry model =
  let
    known = map _.id entry.upserts <> entry.removes
    upserts' = map (reconcileNode known model) entry.upserts
    roots' = map (\rs -> mergeSiblings known rs model.roots) entry.roots
    applied = entry { upserts = upserts', roots = roots' }
    actions = map RemoveTab (Array.mapMaybe (liveTab model) entry.removes)
    inverse = inversePatch now model applied
  in
    { model: applyPatch applied model, patch: applied, inverse, actions }

-- | Which commands record an undo entry. Collapse is a view toggle (kept off the
-- | stack so Ctrl+Z doesn't just flip folds); Activate/CloseNode are browser
-- | actions whose model change, if any, arrives via reconcile rather than a
-- | command patch. The rest are structural/content edits worth reverting.
undoable :: Command -> Boolean
undoable = case _ of
  Collapse _ _ -> false
  Activate _ -> false
  CloseNode _ -> false
  Rename _ _ -> true
  Delete _ -> true
  Move _ _ _ -> true
  Flatten _ -> true
  NewGroup _ _ -> true
  Import _ -> true
