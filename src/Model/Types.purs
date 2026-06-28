-- | The whole data model. A forest of nodes; liveness is an attribute, not a
-- | separate structure. This is the small core the original buried under
-- | reconciliation/journal/projection machinery.
module Model.Types where

import Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set

type NodeId = String

-- | A live browser tab/window as reported by `windows.getAll`. Lives here (a
-- | pure module) so the reducer and the startup re-match can use it without
-- | depending on the Effect layer.
type RuntimeTab =
  { tabId :: Int
  , windowId :: Int
  , index :: Int
  , url :: Maybe String
  , title :: String
  , active :: Boolean
  , favIconUrl :: Maybe String
  }

type RuntimeWindow = { windowId :: Int, tabs :: Array RuntimeTab }

-- | A container node awaiting a freshly-opened browser window to bind to, plus the
-- | EXACT closed tab nodes (in creation order) to rebind in that window. Carrying
-- | the precise list — rather than re-deriving "all the container's closed
-- | children" when the window opens — is what lets a partial restore (one tab out
-- | of a saved group) and a live-tab rehome (which recreates none of the group's
-- | saved tabs) avoid hijacking the container's other saved tabs. `tabs` is empty
-- | for a rehome (the window binds; its dragged tab arrives via onAttached).
type PendingWindow = { node :: NodeId, tabs :: List NodeId }

-- | A node is a browser tab or a container (group/folder). A container is also a
-- | browser *window* whenever it currently owns a live tab — "window" is not a
-- | separate kind, just a container with a live immediate child (and a windowId
-- | binding). See `Model.Tree.isLiveWindow`.
data Kind = KTab | KGroup

derive instance eqKind :: Eq Kind
derive instance ordKind :: Ord Kind
instance showKind :: Show Kind where
  show KTab = "KTab"
  show KGroup = "KGroup"

-- | One node. Liveness is not stored — it is exactly the presence of a browser
-- | binding: `tabId` on a live tab, `windowId` on a container that currently is a
-- | live window. Both are `Nothing` once the object is gone (closed/restorable).
-- | These bindings are how live browser events find their node in O(1) (via the
-- | Model indexes), and the source of truth for `isLive`.
-- |
-- | `children` is the ordered child list owned by the parent. A structural edit
-- | (open/move/delete a child) rewrites and persists that one list, so it costs
-- | O(siblings of the touched node) — bounded by a window's tab count, never
-- | O(total nodes). That is the deliberate trade for a dead-simple model.
type Node =
  { id :: NodeId
  , kind :: Kind
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
  -- | True when this node was reopened out of saved/closed history by a user
  -- | restore (set in `Model.Command.restore`, on exactly the tabs it reopens), as
  -- | opposed to a tab the browser opened fresh. It governs one thing: on a
  -- | *browser*-initiated close, a tab keeps its place in the tree as closed history
  -- | ONLY if this is true (a restored tab belongs in the tree); a freshly-opened
  -- | tab is dropped, not auto-saved. Cleared when the node goes closed again
  -- | (`closeNode`). PERSISTED (unlike the pending-restore queues): because the
  -- | default is now "drop on close", losing this across an event-page suspend would
  -- | wrongly DROP a restored tab — data loss — so it must survive a reload.
  , restoredFromClosed :: Boolean
  }

-- | The authoritative state. `byTab`/`byWindow` are derived indexes giving
-- | O(1) event handling; `pendingRestore` re-binds a restored closed tab to its
-- | existing node instead of spawning a duplicate — keyed by the window the tab
-- | is being recreated in (a FIFO consumed in creation order), since the url the
-- | browser reports for the new tab can differ from the saved one (trailing slash,
-- | redirect, about:blank while loading); `pendingRestoreWindows` is the
-- | FIFO of container nodes awaiting a newly-opened browser window to bind to,
-- | each carrying the exact tabs to rebind there (see `PendingWindow`) — either a
-- | closed window/group being restored, or a saved/plain container a live tab was
-- | just dragged into (so it goes live in place rather than a fresh window node
-- | appearing); `nextId` allocates NodeIds.
type Model =
  { roots :: Array NodeId
  , nodes :: Map NodeId Node
  , byTab :: Map Int NodeId
  , byWindow :: Map Int NodeId
  , pendingRestore :: Map Int (List NodeId)
  , pendingRestoreWindows :: Array PendingWindow
  -- | tabIds the outliner itself is closing (a `CloseNode` "save & close" emits
  -- | `RemoveTab` for each). The browser reports every close — outliner-driven or
  -- | not — as the same `tabs.onRemoved`, so this set is how `TabClosed` tells the
  -- | two apart: a tabId in here is an outliner close (keep it as history, the
  -- | original behaviour) and the marker is consumed; a tabId absent is a genuine
  -- | browser close (subject to the restored-tab drop rule). Transient, like the
  -- | pending-restore queues.
  , closingTabs :: Set Int
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
  , pendingRestoreWindows: []
  , closingTabs: Set.empty
  , nextId: 1
  }

defaultNode :: NodeId -> Kind -> Number -> Node
defaultNode id kind now =
  { id
  , kind
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
  , restoredFromClosed: false
  }

displayTitle :: Node -> String
displayTitle n = fromMaybe n.title n.customTitle

-- | A node is live when it currently mirrors a real browser object: a tab bound
-- | to a `tabId`, or a container bound (as a window) to a `windowId`. Liveness is
-- | not stored — it is exactly the presence of that binding.
isLive :: Node -> Boolean
isLive n = isJust n.tabId || isJust n.windowId

-- | A live tab: bound to a browser tab. (A container never carries a `tabId`.)
isLiveTab :: Node -> Boolean
isLiveTab n = isJust n.tabId
