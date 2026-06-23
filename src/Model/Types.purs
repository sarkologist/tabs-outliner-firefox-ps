-- | The whole data model. A forest of nodes; liveness is an attribute, not a
-- | separate structure. This is the small core the original buried under
-- | reconciliation/journal/projection machinery.
module Model.Types where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)

type NodeId = String

-- | A node is a browser window, a browser tab, or a user-made group (folder).
data Kind = KWindow | KTab | KGroup

derive instance eqKind :: Eq Kind
derive instance ordKind :: Ord Kind
instance showKind :: Show Kind where
  show KWindow = "KWindow"
  show KTab = "KTab"
  show KGroup = "KGroup"

-- | Live = currently mirrors a real browser tab/window. Closed = kept as
-- | history (restorable). Groups are always Live (they have no browser object).
data Status = Live | Closed

derive instance eqStatus :: Eq Status
derive instance ordStatus :: Ord Status
instance showStatus :: Show Status where
  show Live = "Live"
  show Closed = "Closed"

-- | One node. `tabId` is set only on Live KTab nodes; `windowId` only on Live
-- | KWindow nodes (Nothing once Closed). The tree is the source of truth for
-- | structure; these bindings are how live browser events find their node in
-- | O(1) (via the Model indexes).
-- |
-- | `children` is the ordered child list owned by the parent. A structural edit
-- | (open/move/delete a child) rewrites and persists that one list, so it costs
-- | O(siblings of the touched node) — bounded by a window's tab count, never
-- | O(total nodes). That is the deliberate trade for a dead-simple model.
type Node =
  { id :: NodeId
  , kind :: Kind
  , status :: Status
  , parent :: Maybe NodeId
  , children :: Array NodeId
  , title :: String
  , customTitle :: Maybe String -- user rename; overrides title for display
  , url :: Maybe String
  , favIconUrl :: Maybe String
  , active :: Boolean -- live tab that is the active tab in its window
  , collapsed :: Boolean
  , createdAt :: Number
  , closedAt :: Maybe Number
  , tabId :: Maybe Int
  , windowId :: Maybe Int
  , sessionId :: Maybe String -- for browser.sessions restore of closed items
  }

-- | The authoritative state. `byTab`/`byWindow` are derived indexes giving
-- | O(1) event handling; `pendingRestore` re-binds a restored closed tab to its
-- | existing node instead of spawning a duplicate; `nextId` allocates NodeIds.
type Model =
  { roots :: Array NodeId
  , nodes :: Map NodeId Node
  , byTab :: Map Int NodeId
  , byWindow :: Map Int NodeId
  , pendingRestore :: Map String NodeId
  , nextId :: Int
  }

-- | The unit of change. Any model mutation is expressible as node upserts +
-- | removals + an optional new root list. This single shape drives BOTH
-- | persistence (write touched records) and sidebar sync (broadcast + apply),
-- | so both stay O(change) and consistent by construction.
type Patch =
  { upserts :: Array Node
  , removes :: Array NodeId
  , roots :: Maybe (Array NodeId)
  }

-- | Result of applying one input: the new model (authority side) and the patch
-- | (to persist + broadcast).
type Step = { model :: Model, patch :: Patch }

emptyPatch :: Patch
emptyPatch = { upserts: [], removes: [], roots: Nothing }

emptyModel :: Model
emptyModel =
  { roots: []
  , nodes: Map.empty
  , byTab: Map.empty
  , byWindow: Map.empty
  , pendingRestore: Map.empty
  , nextId: 1
  }

defaultNode :: NodeId -> Kind -> Number -> Node
defaultNode id kind now =
  { id
  , kind
  , status: Live
  , parent: Nothing
  , children: []
  , title: ""
  , customTitle: Nothing
  , url: Nothing
  , favIconUrl: Nothing
  , active: false
  , collapsed: false
  , createdAt: now
  , closedAt: Nothing
  , tabId: Nothing
  , windowId: Nothing
  , sessionId: Nothing
  }

displayTitle :: Node -> String
displayTitle n = fromMaybe n.title n.customTitle
