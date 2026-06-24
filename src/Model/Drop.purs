-- | Pure placement for the drag-and-drop landing preview: given the visible row
-- | list, where will the dragged node come to rest? Expressed as a horizontal
-- | insertion line drawn at the top edge of visible row `atIndex`, indented to
-- | `depth` — the visual analogue of `Sidebar.Main`'s `dropCommand`, so the
-- | preview is exactly what a drop would do. Kept pure (and tested) here.
module Model.Drop where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Model.Command (Command(..))
import Model.Guide (subtreeEnd)
import Model.Tree (Entry, isAncestorOrSelf)
import Model.Types (Kind(..), Model, Node, NodeId)

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

-- | The move a drop performs — the behavioural counterpart of `dropPlacement`,
-- | kept beside it so the preview and the action can't drift. Onto a group:
-- | append as its last child. Onto anything else: place immediately before the
-- | target as a sibling. `move` deletes the dragged node before inserting, so the
-- | index is the target's position in the sibling list with the node removed —
-- | which is what makes a same-parent downward drag land before the target, not
-- | after it.
dropCommand :: NodeId -> Node -> Model -> Command
dropCommand dragId target model = case target.kind of
  KGroup -> Move dragId (Just target.id) (Array.length target.children)
  _ -> Move dragId target.parent (beforeIndex dragId target.id target.parent model)

-- index at which inserting lands the dragged node immediately before `targetId`,
-- accounting for the node's own removal from the list
beforeIndex :: NodeId -> NodeId -> Maybe NodeId -> Model -> Int
beforeIndex dragId targetId mParent model =
  fromMaybe (Array.length shrunk) (Array.elemIndex targetId shrunk)
  where
  siblings = case mParent of
    Just pid -> fromMaybe [] (_.children <$> Map.lookup pid model.nodes)
    Nothing -> model.roots
  shrunk = Array.delete dragId siblings
