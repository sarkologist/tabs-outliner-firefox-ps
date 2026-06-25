-- | Pure placement for the drag-and-drop landing preview: where will the dragged
-- | node come to rest? Expressed as a horizontal insertion line at the top edge of
-- | visible row `atIndex`, indented to `depth`. Works off the projected `ViewRow`s
-- | alone (no model) — the behavioural twin is the `Drop` case in `Model.Command`,
-- | so the preview is exactly what a drop would do.
module Model.Drop (Placement, dropPlacement) where

import Prelude

import Data.Maybe (Maybe(..))
import Model.Types (Kind(..))
import Model.View (ViewRow)

-- A horizontal insertion line at the top edge of visible row `atIndex`, indented
-- to `depth` (the depth the dropped node's icon will sit at).
type Placement = { atIndex :: Int, depth :: Int }

-- | Where dragging the node that spans visible indices `[drag.index,
-- | drag.subtreeEnd)` onto `target` lands, or `Nothing` when it's a no-op (onto
-- | itself or into its own subtree — the cycle guard, by flat-index containment).
-- | Onto a group: a line after the group's whole visible subtree, one level
-- | deeper. Onto anything else: a sibling line at the target's depth.
dropPlacement :: { index :: Int, subtreeEnd :: Int } -> ViewRow -> Maybe Placement
dropPlacement drag target
  | target.index >= drag.index && target.index < drag.subtreeEnd = Nothing
  | otherwise = Just case target.kind of
      KGroup -> { atIndex: target.subtreeEnd, depth: target.depth + 1 }
      _ -> { atIndex: target.index, depth: target.depth }
