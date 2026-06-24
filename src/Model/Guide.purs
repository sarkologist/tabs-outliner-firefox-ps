-- | Pure geometry for the sidebar's hover guide: given the flat preorder list of
-- | visible rows and the index of the hovered row, compute the guide lines that
-- | trace the hovered node's subtree — a vertical line connecting it up to its
-- | parent, vertical connectors threading down each level of the subtree, and a
-- | horizontal stub into every visible descendant. O(subtree), capped.
-- |
-- | Reproduces the original extension's hover guide; kept here (a pure module) so
-- | it is unit-tested independently of the Halogen view that renders it.
module Model.Guide where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int.Bits (or)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Model.Tree (Entry)

-- | Per-row guide overlay, keyed by a row's flat preorder index: vertical line
-- | segments (depth -> segment-flag bitset) and an optional horizontal connector
-- | depth.
type Guide = { verticals :: Map Int (Map Int Int), horizontals :: Map Int Int }

emptyGuide :: Guide
emptyGuide = { verticals: Map.empty, horizontals: Map.empty }

-- which half (or both) of a row a vertical line spans
guideTop :: Int
guideTop = 1

guideBottom :: Int
guideBottom = 2

guideFull :: Int
guideFull = 3

-- Cap the subtree size we draw guides for, so hovering a huge expanded group
-- doesn't spray thousands of line elements (mirrors the original's guard).
maxGuideRows :: Int
maxGuideRows = 1000

-- | Build the guide for the row hovered at flat preorder index `h`. Pure over the
-- | `entries` list; O(subtree), and skipped entirely past the cap.
buildGuide :: Array Entry -> Int -> Guide
buildGuide entries h = case Array.index entries h of
  Nothing -> emptyGuide
  Just target ->
    let
      -- Bounded scan: stop at the cap, so hovering a giant subtree costs
      -- O(maxGuideRows), never O(subtree).
      end = subtreeEnd entries target.depth (h + maxGuideRows + 1) (h + 1)
    in
      if end - h > maxGuideRows then emptyGuide
      else
        let
          hParent = parentOf entries h target.depth
          acc = foldl (step hParent)
            { conn: Map.empty, verticals: Map.empty, horizontals: Map.empty }
            (Array.range h (end - 1))
        in
          { verticals: acc.verticals, horizontals: acc.horizontals }
  where
  -- One pass over the subtree. `conn` maps depth -> the previous connector row
  -- seen at that depth (index + parent). A vertical line is extended only by the
  -- gap since its previous sibling — so a row is filled once, making the pass
  -- O(subtree) instead of re-filling the whole span from the parent per child.
  step hParent acc c = case Array.index entries c of
    Nothing -> acc
    Just e ->
      let
        dd = e.depth
        parent = if c == h then hParent else _.idx <$> Map.lookup (dd - 1) acc.conn
        horizontals' = if dd > 0 then Map.insert c dd acc.horizontals else acc.horizontals
        verticals' = case parent of
          Just p | dd > 0 -> case Map.lookup dd acc.conn of
            -- another child of the same parent: continue the line down through
            -- the previous sibling (now a pass-through) and the gap to here
            Just prev | prev.parent == p ->
              addSeg (fillGap (addSeg acc.verticals prev.idx dd guideFull) (prev.idx + 1) (c - 1) dd) c dd guideTop
            -- first child of this parent: open the line at the parent's bottom
            _ -> addSeg (addSeg acc.verticals p dd guideBottom) c dd guideTop
          _ -> acc.verticals
        conn' = Map.insert dd { idx: c, parent: fromMaybe (-1) parent } acc.conn
      in
        { conn: conn', verticals: verticals', horizontals: horizontals' }

  fillGap acc lo hi dd
    | lo > hi = acc
    | otherwise = foldl (\a m -> addSeg a m dd guideFull) acc (Array.range lo hi)

-- exclusive end of the subtree whose root has depth `d`, scanning from `i` but
-- never past `limit` (the caller's cap, so a huge subtree isn't fully scanned)
subtreeEnd :: Array Entry -> Int -> Int -> Int -> Int
subtreeEnd entries d limit i
  | i >= limit = i
  | otherwise = case Array.index entries i of
      Just e | e.depth > d -> subtreeEnd entries d limit (i + 1)
      _ -> i

-- index of the parent row (nearest earlier row one level shallower), if any
parentOf :: Array Entry -> Int -> Int -> Maybe Int
parentOf entries h d
  | d <= 0 = Nothing
  | otherwise = go (h - 1)
      where
      go i
        | i < 0 = Nothing
        | otherwise = case Array.index entries i of
            Just e | e.depth < d -> Just i
            Just _ -> go (i - 1)
            Nothing -> Nothing

-- OR a vertical segment flag into (row -> depth -> flags)
addSeg :: Map Int (Map Int Int) -> Int -> Int -> Int -> Map Int (Map Int Int)
addSeg acc row depth seg =
  Map.insertWith (Map.unionWith or) row (Map.singleton depth seg) acc
