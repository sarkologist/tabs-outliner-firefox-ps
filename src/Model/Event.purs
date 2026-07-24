-- | The browser-side inputs the reducer reacts to. A deliberately small subset
-- | of the WebExtension event surface — enough for live mirroring, no more.
module Model.Event where

import Data.Maybe (Maybe)
import Model.Types (NodeId)

type OpenedTab =
  { tabId :: Int
  , windowId :: Int
  , openerTabId :: Maybe Int
  , index :: Int
  , url :: Maybe String
  , title :: String
  , active :: Boolean
  , favIconUrl :: Maybe String
  }

data BrowserEvent
  = WindowOpened { windowId :: Int }
  -- | A window we just created for a restore/rehome has opened, and the impure
  -- | layer paired it (by creation order) to the exact container node it should
  -- | bind to. Unlike `WindowOpened`, this names the node, so binding no longer
  -- | depends on the shared FIFO's head: two windows restored at once (or a stale
  -- | queue entry) can't cross-wire, and the wrong tab can't rebind into it.
  | WindowBound { node :: NodeId, windowId :: Int }
  -- | A window we asked the browser to create never opened (the create rejected).
  -- | Counterpart to `WindowBound`: it names the container that will now never be
  -- | bound, so the reducer can retract that container's pending-window entry.
  | WindowCreateFailed { node :: NodeId }
  -- | A tab we asked the browser to create in `windowId` never opened. Names the
  -- | node that was queued to rebind to it, so that queue slot can be dropped —
  -- | the queue is consumed in creation order, so a slot left behind would rebind
  -- | the NEXT tab opened in that window onto this node.
  | TabCreateFailed { windowId :: Int, node :: NodeId }
  | WindowClosed { windowId :: Int }
  | TabOpened OpenedTab
  | TabClosed { tabId :: Int }
  | TabChanged
      { tabId :: Int
      , url :: Maybe String
      , title :: Maybe String
      , favIconUrl :: Maybe String
      }
  | TabActivated { tabId :: Int, windowId :: Int }
  | TabMoved { tabId :: Int, windowId :: Int, toIndex :: Int }
  | TabAttached { tabId :: Int, windowId :: Int, index :: Int }
