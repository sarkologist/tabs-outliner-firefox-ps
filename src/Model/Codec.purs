-- | JSON serialization for Node / Patch / snapshot, shared by persistence
-- | (stringified per record) and the message channel (Json over runtime
-- | messaging). A plain JSON-friendly NodeRec mirror keeps Kind as a string so
-- | argonaut's generic record codec does the work — no hand-written instances,
-- | no structured-cloning of PureScript ADTs. Liveness isn't serialized at all:
-- | it is re-derived from the tabId/windowId bindings the record carries.
module Model.Codec
  ( encodeNode
  , decodeNode
  , encodePatch
  , decodePatch
  , encodeSnapshot
  , encodeSnapshotData
  , decodeSnapshot
  , Snapshot
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
import Model.Types (Kind(..), Model, Node, NodeId, Patch)

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

encodePatch :: Patch -> Json
encodePatch p = encodeJson { upserts: map toRec p.upserts, removes: p.removes, roots: p.roots }

decodePatch :: Json -> Either String Patch
decodePatch json = do
  rec <- lmap printJsonDecodeError
    (decodeJson json :: Either _ { upserts :: Array NodeRec, removes :: Array NodeId, roots :: Maybe (Array NodeId) })
  pure { upserts: map fromRec rec.upserts, removes: rec.removes, roots: rec.roots }

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
  }

kindStr :: Kind -> String
kindStr KTab = "tab"
kindStr KGroup = "group"

-- | MIGRATION (removable once persisted data has been rewritten once): legacy
-- | records used kind "window" for live/saved browser windows; those are now just
-- | containers, so map "window" — and any unknown kind — to a group. Legacy
-- | records also carried a "status" field, which is simply ignored on decode
-- | (argonaut drops unrecognized object keys). Liveness is re-derived from the
-- | tabId/windowId bindings the record already has, and re-confirmed by the
-- | startup re-match against the real browser.
parseKind :: String -> Kind
parseKind "tab" = KTab
parseKind "group" = KGroup
parseKind "window" = KGroup -- legacy: a browser window is now just a container
parseKind _ = KGroup -- unknown kind: keep the node (as a container) rather than drop it
