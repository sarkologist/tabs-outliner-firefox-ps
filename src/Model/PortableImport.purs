-- | Convert the original "Tab Session Outliner" portable-tree export into our
-- | flat snapshot. That export is a NESTED tree
-- |   { schema:"tabs-outliner-tree", version, roots:[ {kind,title,url,children} ] }
-- | whereas our own export is a FLAT { nodes:[…], roots:[ids] }. Windows/groups
-- | become folders and tabs become tab nodes; the Import command then makes the
-- | whole thing inert, restorable history.
-- |
-- | O(nodes): the node accumulator is consed (real exports run to tens of
-- | thousands of nodes, so no quadratic array concatenation).
module Model.PortableImport (portableToSnapshot) where

import Prelude

import Control.Alternative (guard)
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Model.Codec (Snapshot)
import Model.Types (Kind(..), Node, NodeId, defaultNode)

type St = { nodes :: List Node, next :: Int }

portableToSnapshot :: Json -> Maybe Snapshot
portableToSnapshot json = do
  obj <- toObject json
  schema <- Object.lookup "schema" obj >>= toString
  guard (schema == "tabs-outliner-tree")
  rootsJ <- Object.lookup "roots" obj >>= toArray
  let res = walkForest Nothing rootsJ { nodes: Nil, next: 0 }
  pure { nodes: Array.fromFoldable res.st.nodes, roots: res.ids }

-- Walk a forest of portable nodes left-to-right, threading the id counter and
-- the (consed) node accumulator. Returns the child ids and the updated state.
walkForest :: Maybe NodeId -> Array Json -> St -> { ids :: Array NodeId, st :: St }
walkForest parent js st0 = foldl step { ids: [], st: st0 } js
  where
  step acc j = case walkOne parent j acc.st of
    Just (Tuple id st') -> { ids: Array.snoc acc.ids id, st: st' }
    Nothing -> acc

walkOne :: Maybe NodeId -> Json -> St -> Maybe (Tuple NodeId St)
walkOne parent j st = do
  obj <- toObject j
  let
    id = "p" <> show st.next
    field k = Object.lookup k obj >>= toString
    kind = if fromMaybe "tab" (field "kind") == "tab" then KTab else KGroup
    childRes = walkForest (Just id) (fromMaybe [] (Object.lookup "children" obj >>= toArray)) (st { next = st.next + 1 })
    node = (defaultNode id kind 0.0)
      { title = fromMaybe "" (field "title")
      , customTitle = field "customTitle"
      , url = field "url"
      , favIconUrl = field "favIconUrl"
      , parent = parent
      , children = childRes.ids
      }
  pure (Tuple id (childRes.st { nodes = Cons node childRes.st.nodes }))
