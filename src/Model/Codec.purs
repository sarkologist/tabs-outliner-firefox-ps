-- | JSON serialization for Node / Patch / snapshot, shared by persistence
-- | (stringified per record) and the message channel (Json over runtime
-- | messaging). A plain JSON-friendly NodeRec mirror keeps Kind as a string so
-- | argonaut's generic record codec does the work — no hand-written instances,
-- | no structured-cloning of PureScript ADTs. Liveness isn't serialized at all:
-- | it is re-derived from the tabId/windowId bindings the record carries.
module Model.Codec
  ( encodeNode
  , decodeNode
  , encodeSnapshot
  , encodeSnapshotData
  , decodeSnapshot
  , Snapshot
  , kindStr
  , parseKind
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Model.Types (Kind(..), Model, Node, NodeId)

type NodeRec =
  { id :: String
  , kind :: String
  , parent :: Maybe String
  , children :: Array String
  , title :: String
  , customTitle :: Maybe String
  , url :: Maybe String
  , favIconUrl :: Maybe String
  , active :: Boolean
  , collapsed :: Boolean
  , createdAt :: Number
  , closedAt :: Maybe Number
  , tabId :: Maybe Int
  , windowId :: Maybe Int
  , sessionId :: Maybe String
  }

type Snapshot = { nodes :: Array Node, roots :: Array NodeId }

encodeNode :: Node -> Json
encodeNode = encodeJson <<< toRec

decodeNode :: Json -> Either String Node
decodeNode json = map fromRec (lmap printJsonDecodeError (decodeJson json))

encodeSnapshot :: Model -> Json
encodeSnapshot model = encodeJson
  { nodes: map toRec (Array.fromFoldable (Map.values model.nodes))
  , roots: model.roots
  }

encodeSnapshotData :: Snapshot -> Json
encodeSnapshotData s = encodeJson { nodes: map toRec s.nodes, roots: s.roots }

decodeSnapshot :: Json -> Either String Snapshot
decodeSnapshot json = do
  rec <- lmap printJsonDecodeError
    (decodeJson json :: Either _ { nodes :: Array NodeRec, roots :: Array NodeId })
  pure { nodes: map fromRec rec.nodes, roots: rec.roots }

toRec :: Node -> NodeRec
toRec n =
  { id: n.id
  , kind: kindStr n.kind
  , parent: n.parent
  , children: n.children
  , title: n.title
  , customTitle: n.customTitle
  , url: n.url
  , favIconUrl: n.favIconUrl
  , active: n.active
  , collapsed: n.collapsed
  , createdAt: n.createdAt
  , closedAt: n.closedAt
  , tabId: n.tabId
  , windowId: n.windowId
  , sessionId: n.sessionId
  }

fromRec :: NodeRec -> Node
fromRec r =
  { id: r.id
  , kind: parseKind r.kind
  , parent: r.parent
  , children: r.children
  , title: r.title
  , customTitle: r.customTitle
  , url: r.url
  , favIconUrl: r.favIconUrl
  , active: r.active
  , collapsed: r.collapsed
  , createdAt: r.createdAt
  , closedAt: r.closedAt
  , tabId: r.tabId
  , windowId: r.windowId
  , sessionId: r.sessionId
  -- not serialized: restore origin is transient runtime state, re-derived false on
  -- load (a reloaded live tab is treated as a fresh binding, not a user restore).
  , restoredFromClosed: false
  }

kindStr :: Kind -> String
kindStr KTab = "tab"
kindStr KGroup = "group"

-- | A node is a tab or a container; every non-"tab" kind decodes to a container.
-- | (Old records that predate the window/group unification used "tab", "group",
-- | and "window" — the last just falls into the container default, so they still
-- | load. A decode never fails on the kind, so a record is never dropped.)
parseKind :: String -> Kind
parseKind "tab" = KTab
parseKind _ = KGroup
