-- | Undo/redo as inverse patches. Every model-mutating user command already
-- | produces a `Patch`; the inverse of that patch — computed against the model as
-- | it was *before* the command — is the patch that reverts it. The background
-- | keeps a stack of these inverses, so undo/redo cost O(change) and reuse the
-- | very same persist + broadcast path a command does: no snapshots, no second
-- | way to apply a change, the view and the authority stay consistent by
-- | construction.
-- |
-- | Two wrinkles the inverse handles so undo stays faithful:
-- |   * A live tab/window the command *removed* (Delete) can't have its browser
-- |     object resurrected by undo, so it comes back as Closed history — the same
-- |     restorable state a close would have left. Groups have no browser object,
-- |     so they come back unchanged.
-- |   * `roots` is restored absolutely, but merged with any roots that appeared
-- |     since (e.g. a window opened between the command and the undo) so undo
-- |     never drops an unrelated new window out of the forest.
module Model.Undo
  ( inversePatch
  , mergeRoots
  , applyEntry
  , undoable
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Command (Command(..))
import Model.Tree (applyPatch)
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

-- | The effective `roots` for applying a stored entry to the *current* model:
-- | the entry's roots, plus any current root the entry neither mentions nor
-- | removes — so a window opened since the entry was recorded is not dropped.
mergeRoots :: Patch -> Model -> Maybe (Array NodeId)
mergeRoots entry model = case entry.roots of
  Nothing -> Nothing
  Just rs ->
    let keep r = not (Array.elem r rs) && not (Array.elem r entry.removes)
    in Just (rs <> Array.filter keep model.roots)

type Applied = { model :: Model, patch :: Patch, inverse :: Patch }

-- | Apply a stored undo/redo entry to the current model. Returns the patch
-- | actually applied (roots merged — to persist + broadcast) and its inverse (to
-- | push on the opposite stack). Reuses `applyPatch`, so undo/redo move the model
-- | exactly the way a command does.
applyEntry :: Number -> Patch -> Model -> Applied
applyEntry now entry model =
  let
    applied = entry { roots = mergeRoots entry model }
    inverse = inversePatch now model applied
  in
    { model: applyPatch applied model, patch: applied, inverse }

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
