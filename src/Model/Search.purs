-- | Pure search helpers shared by the model projection and sidebar rendering.
-- | Matching follows the original extension's behavior: trim the query, compare
-- | case-insensitively, and search both the display title and URL.
module Model.Search
  ( SearchTextSegment
  , matchesSearch
  , normalizeSearchQuery
  , segmentSearchText
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.String.Pattern (Pattern(..))
import Model.Types (Node, displayTitle)

type SearchTextSegment = { text :: String, isMatch :: Boolean }

normalizeSearchQuery :: String -> String
normalizeSearchQuery = String.toLower <<< String.trim

matchesSearch :: String -> Node -> Boolean
matchesSearch rawQuery n =
  let
    q = normalizeSearchQuery rawQuery
    contains = String.contains (String.Pattern q) <<< String.toLower
  in
    q /= "" && (contains (displayTitle n) || maybe false contains n.url)

segmentSearchText :: String -> String -> Array SearchTextSegment
segmentSearchText text rawQuery =
  let
    q = normalizeSearchQuery rawQuery
  in
    if q == "" then plain text
    else go q text (String.toLower text) (CU.length q) 0 []

plain :: String -> Array SearchTextSegment
plain text = if text == "" then [] else [ { text, isMatch: false } ]

slice :: Int -> Int -> String -> String
slice start end = CU.take (end - start) <<< CU.drop start

go :: String -> String -> String -> Int -> Int -> Array SearchTextSegment -> Array SearchTextSegment
go q text lowerText qLen cursor acc = case CU.indexOf' (Pattern q) cursor lowerText of
  Nothing ->
    let rest = CU.drop cursor text
    in if rest == "" then acc else Array.snoc acc { text: rest, isMatch: false }
  Just matchStart ->
    let
      matchEnd = matchStart + qLen
      withBefore =
        if matchStart > cursor then Array.snoc acc { text: slice cursor matchStart text, isMatch: false }
        else acc
    in
      go q text lowerText qLen matchEnd (Array.snoc withBefore { text: slice matchStart matchEnd text, isMatch: true })
