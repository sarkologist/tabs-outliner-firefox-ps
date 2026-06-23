-- | The single seam to the WebExtension API. Everything reaches the browser
-- | through this capability over `globalThis.browser`; in tests that global is
-- | a fake, so the exact same code runs in Firefox and under Playwright.
module Effect.Browser
  ( BrowserApi
  , RuntimeTab
  , RuntimeWindow
  , getBrowser
  , getAllWindows
  , subscribe
  , focusTab
  , createTab
  , removeTab
  , restoreSession
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Model.Event (BrowserEvent(..))

foreign import data BrowserApi :: Type

foreign import getBrowser :: Effect BrowserApi

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

foreign import focusTabImpl :: BrowserApi -> Int -> Int -> Effect (Promise Unit)
foreign import createTabImpl :: BrowserApi -> Nullable Int -> Nullable String -> Effect (Promise Unit)
foreign import removeTabImpl :: BrowserApi -> Int -> Effect (Promise Unit)
foreign import restoreSessionImpl :: BrowserApi -> String -> Effect (Promise Unit)

focusTab :: BrowserApi -> Int -> Int -> Aff Unit
focusTab api windowId tabId = toAffE (focusTabImpl api windowId tabId)

createTab :: BrowserApi -> Maybe Int -> Maybe String -> Aff Unit
createTab api windowId url = toAffE (createTabImpl api (toNullable windowId) (toNullable url))

removeTab :: BrowserApi -> Int -> Aff Unit
removeTab api tabId = toAffE (removeTabImpl api tabId)

restoreSession :: BrowserApi -> String -> Aff Unit
restoreSession api sessionId = toAffE (restoreSessionImpl api sessionId)
