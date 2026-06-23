-- | The browser-side inputs the reducer reacts to. A deliberately small subset
-- | of the WebExtension event surface — enough for live mirroring, no more.
module Model.Event where

import Data.Maybe (Maybe)

type OpenedTab =
  { tabId :: Int
  , windowId :: Int
  , index :: Int
  , url :: Maybe String
  , title :: String
  , active :: Boolean
  , favIconUrl :: Maybe String
  }

data BrowserEvent
  = WindowOpened { windowId :: Int }
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
