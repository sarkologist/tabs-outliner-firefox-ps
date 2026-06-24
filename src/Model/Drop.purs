-- | Pure placement for the drag-and-drop landing preview: given the visible row
-- | list, where will the dragged node come to rest? Expressed as a horizontal
-- | insertion line drawn at the top edge of visible row `atIndex`, indented to
-- | `depth` — the visual analogue of `Sidebar.Main`'s `dropCommand`, so the
-- | preview is exactly what a drop would do. Kept pure (and tested) here.
module Model.Drop where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Model.Guide (subtreeEnd)
import Model.Tree (Entry, isAncestorOrSelf)
import Model.Types (Kind(..), Model, NodeId)

-- A horizontal insertion line at the top edge of visible row `atIndex`, indented
-- to `depth` (the depth the dropped node's icon will sit at).
type Placement = { atIndex :: Int, depth :: Int }

-- | Where dragging `dragId` onto `targetId` would land, or `Nothing` when the
-- | drop is a no-op (onto itself or into its own subtree) — matching the cycle
-- | guard in `Model.Command`'s move. Onto a group: appended as its last child
-- | (a line below the group's visible subtree, one level deeper). Onto anything
-- | else: inserted before the target as a sibling (a line at the target's depth).
dropPlacement :: Model -> Array Entry -> NodeId -> NodeId -> Maybe Placement
dropPlacement model entries dragId targetId
  | isAncestorOrSelf dragId targetId model = Nothing
  | otherwise = case Map.lookup targetId model.nodes of
      Nothing -> Nothing
      Just target -> case Array.findIndex (\e -> e.id == targetId) entries of
        Nothing -> Nothing
        Just ti ->
          let
            td = maybe 0 _.depth (Array.index entries ti)
          in
            Just case target.kind of
              KGroup -> { atIndex: subtreeEnd entries td (Array.length entries) (ti + 1), depth: td + 1 }
              _ -> { atIndex: ti, depth: td }
