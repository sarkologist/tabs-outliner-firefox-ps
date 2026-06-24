-- | The single seam to the WebExtension API. Everything reaches the browser
-- | through this capability over `globalThis.browser`; in tests that global is
-- | a fake, so the exact same code runs in Firefox and under Playwright.
module Effect.Browser
  ( BrowserApi
  , getBrowser
  , getAllWindows
  , getCurrentWindowId
  , subscribe
  , focusTab
  , createTab
  , createWindow
  , removeTab
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Model.Event (BrowserEvent(..))
import Model.Types (RuntimeWindow)

foreign import data BrowserApi :: Type

foreign import getBrowser :: Effect BrowserApi

type RawTab =
  { tabId :: Int
  , windowId :: Int
  , index :: Int
  , url :: Nullable String
  , title :: String
  , active :: Boolean
  , favIconUrl :: Nullable String
  }

type RawWindow = { windowId :: Int, tabs :: Array RawTab }

foreign import getAllWindowsImpl :: BrowserApi -> Effect (Promise (Array RawWindow))

-- | Current browser windows + tabs. Used at boot for the startup re-match.
getAllWindows :: BrowserApi -> Aff (Array RuntimeWindow)
getAllWindows api = map (map cleanWindow) (toAffE (getAllWindowsImpl api))
  where
  cleanWindow w = { windowId: w.windowId, tabs: map cleanTab w.tabs }
  cleanTab t =
    { tabId: t.tabId
    , windowId: t.windowId
    , index: t.index
    , url: toMaybe t.url
    , title: t.title
    , active: t.active
    , favIconUrl: toMaybe t.favIconUrl
    }

foreign import getCurrentWindowIdImpl :: BrowserApi -> Effect (Promise (Nullable Int))

-- | The browser window that hosts this sidebar instance. Firefox shows one
-- | sidebar per window, so this scopes "scroll to the active tab" to the window
-- | the user is actually looking at. `Nothing` if the API is unavailable.
getCurrentWindowId :: BrowserApi -> Aff (Maybe Int)
getCurrentWindowId api = map toMaybe (toAffE (getCurrentWindowIdImpl api))

type RawOpened =
  { tabId :: Int
  , windowId :: Int
  , index :: Int
  , url :: Nullable String
  , title :: String
  , active :: Boolean
  , favIconUrl :: Nullable String
  }

type RawChanged =
  { tabId :: Int
  , url :: Nullable String
  , title :: Nullable String
  , favIconUrl :: Nullable String
  }

type Sink =
  { tabOpened :: RawOpened -> Effect Unit
  , tabClosed :: Int -> Effect Unit
  , tabChanged :: RawChanged -> Effect Unit
  , tabActivated :: { tabId :: Int, windowId :: Int } -> Effect Unit
  , tabMoved :: { tabId :: Int, windowId :: Int, toIndex :: Int } -> Effect Unit
  , tabAttached :: { tabId :: Int, windowId :: Int, index :: Int } -> Effect Unit
  , windowOpened :: Int -> Effect Unit
  , windowClosed :: Int -> Effect Unit
  }

foreign import subscribeImpl :: BrowserApi -> Sink -> Effect Unit

-- | Wire all live browser events into a single typed handler.
subscribe :: BrowserApi -> (BrowserEvent -> Effect Unit) -> Effect Unit
subscribe api handle = subscribeImpl api
  { tabOpened: \r -> handle
      ( TabOpened
          { tabId: r.tabId
          , windowId: r.windowId
          , index: r.index
          , url: toMaybe r.url
          , title: r.title
          , active: r.active
          , favIconUrl: toMaybe r.favIconUrl
          }
      )
  , tabClosed: \t -> handle (TabClosed { tabId: t })
  , tabChanged: \r -> handle
      (TabChanged { tabId: r.tabId, url: toMaybe r.url, title: toMaybe r.title, favIconUrl: toMaybe r.favIconUrl })
  , tabActivated: \r -> handle (TabActivated r)
  , tabMoved: \r -> handle (TabMoved r)
  , tabAttached: \r -> handle (TabAttached r)
  , windowOpened: \w -> handle (WindowOpened { windowId: w })
  , windowClosed: \w -> handle (WindowClosed { windowId: w })
  }

foreign import focusTabImpl :: BrowserApi -> Int -> Effect (Promise Unit)
foreign import createTabImpl :: BrowserApi -> Nullable Int -> Nullable String -> Effect (Promise Unit)
foreign import createWindowImpl :: BrowserApi -> Array String -> Effect (Promise Unit)
foreign import removeTabImpl :: BrowserApi -> Int -> Effect (Promise Unit)

-- | Activate a tab and focus its window (the FFI resolves the window from the tab).
focusTab :: BrowserApi -> Int -> Aff Unit
focusTab api tabId = toAffE (focusTabImpl api tabId)

createTab :: BrowserApi -> Maybe Int -> Maybe String -> Aff Unit
createTab api windowId url = toAffE (createTabImpl api (toNullable windowId) (toNullable url))

-- | Open one new browser window populated with the given urls.
createWindow :: BrowserApi -> Array String -> Aff Unit
createWindow api urls = toAffE (createWindowImpl api urls)

removeTab :: BrowserApi -> Int -> Aff Unit
removeTab api tabId = toAffE (removeTabImpl api tabId)
