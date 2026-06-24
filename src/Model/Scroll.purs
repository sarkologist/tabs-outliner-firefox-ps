-- | Pure logic for "reveal the focused tab": which node a sidebar should scroll
-- | to, and the scroll offset that brings it into view. Firefox shows one sidebar
-- | per browser window, so each instance follows its OWN window's active tab
-- | (mirroring the original, whose primary target is the sidebar window's active
-- | tab). Both functions are pure so they can be unit-tested without a DOM.
module Model.Scroll
  ( ScrollGeom
  , activeTabInWindow
  , revealScrollTop
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Tree (liveWindowNode)
import Model.Types (Kind(..), Model, NodeId, isLiveTab)

-- | The live, active tab inside the live browser window `windowId` — the node the
-- | sidebar reveals for its host window. Preorder, short-circuiting at the first
-- | match, so it costs O(subtree-up-to-the-active-tab), not O(total). It walks the
-- | whole window subtree (not just direct children) so a tab the user grouped
-- | under the window still counts.
activeTabInWindow :: Int -> Model -> Maybe NodeId
activeTabInWindow windowId model = liveWindowNode windowId model >>= \w -> go w.id
  where
  go :: NodeId -> Maybe NodeId
  go id = case Map.lookup id model.nodes of
    Nothing -> Nothing
    Just n
      | n.kind == KTab && isLiveTab n && n.active -> Just n.id
      | otherwise -> Array.findMap go n.children

-- | Geometry of the virtualized tree: row height, the scroll viewport height, the
-- | total scrollable content height, and the current scroll offset — all in px.
type ScrollGeom =
  { rowHeight :: Number
  , viewportHeight :: Number
  , contentHeight :: Number
  , scrollTop :: Number
  }

-- | The scrollTop that centers visible-row `idx`, or `Nothing` when the row is
-- | already fully in view (don't move what the user can already see). The target
-- | is clamped to the scrollable range, so a row near either end aligns to the
-- | edge rather than overscrolling.
revealScrollTop :: ScrollGeom -> Int -> Maybe Number
revealScrollTop g idx =
  let
    rowTop = Int.toNumber idx * g.rowHeight
    rowBottom = rowTop + g.rowHeight
    maxTop = max 0.0 (g.contentHeight - g.viewportHeight)
    centered = rowTop + g.rowHeight / 2.0 - g.viewportHeight / 2.0
  in
    if rowTop >= g.scrollTop && rowBottom <= g.scrollTop + g.viewportHeight then Nothing
    else Just (clamp 0.0 maxTop centered)
