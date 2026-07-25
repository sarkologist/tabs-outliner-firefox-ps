-- | The single seam to the WebExtension API. Everything reaches the browser
-- | through this capability over `globalThis.browser`; in tests that global is
-- | a fake, so the exact same code runs in Firefox and under Playwright.
module Effect.Browser
  ( BrowserApi
  , getBrowser
  , initSidebarAction
  , getAllWindows
  , getCurrentWindowId
  , subscribe
  , focusTab
  , createTab
  , createWindow
  , moveTabToWindow
  , newWindowWithTabs
  , removeTab
  , openFullSizeOutliner
  , tagTab
  , getAutomaticBackupsEnabled
  , setAutomaticBackupsEnabled
  , automaticBackupDue
  , recordAutomaticBackupSuccess
  , ensureBackupAlarm
  , clearBackupAlarm
  , onBackupAlarm
  , backupFilename
  , downloadBackupFile
  , downloadExportFile
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Model.Event (BrowserEvent(..))
import Model.Types (RuntimeWindow)

foreign import data BrowserApi :: Type

foreign import getBrowser :: Effect BrowserApi

foreign import initSidebarActionImpl :: BrowserApi -> Effect Unit

initSidebarAction :: BrowserApi -> Effect Unit
initSidebarAction = initSidebarActionImpl

type RawTab =
  { tabId :: Int
  , windowId :: Int
  , openerTabId :: Nullable Int
  , index :: Int
  , url :: Nullable String
  , title :: String
  , active :: Boolean
  , favIconUrl :: Nullable String
  , nodeKey :: Nullable String
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
    , openerTabId: toMaybe t.openerTabId
    , index: t.index
    , url: toMaybe t.url
    , title: t.title
    , active: t.active
    , favIconUrl: toMaybe t.favIconUrl
    , nodeKey: toMaybe t.nodeKey
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
  , openerTabId :: Nullable Int
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
  , windowBound :: { node :: String, windowId :: Int } -> Effect Unit
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
          , openerTabId: toMaybe r.openerTabId
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
  , windowBound: \r -> handle (WindowBound { node: r.node, windowId: r.windowId })
  , windowClosed: \w -> handle (WindowClosed { windowId: w })
  }

foreign import focusTabImpl :: BrowserApi -> Int -> Effect (Promise Unit)
foreign import createTabImpl :: BrowserApi -> Nullable Int -> Nullable Int -> Nullable String -> Effect (Promise Unit)
foreign import createWindowImpl :: BrowserApi -> String -> Array String -> Effect (Promise Unit)
foreign import moveTabToWindowImpl :: BrowserApi -> Int -> Int -> Int -> Effect (Promise Unit)
foreign import newWindowWithTabsImpl :: BrowserApi -> Nullable String -> Array Int -> Effect (Promise Unit)
foreign import removeTabImpl :: BrowserApi -> Int -> Effect (Promise Unit)
foreign import openFullSizeOutlinerImpl :: BrowserApi -> Nullable Int -> Effect (Promise Unit)
foreign import tagTabImpl :: BrowserApi -> Int -> String -> Effect (Promise Unit)
foreign import getAutomaticBackupsEnabledImpl :: BrowserApi -> Effect (Promise Boolean)
foreign import setAutomaticBackupsEnabledImpl :: BrowserApi -> Boolean -> Effect (Promise Unit)
foreign import automaticBackupDueImpl :: BrowserApi -> Effect (Promise Boolean)
foreign import recordAutomaticBackupSuccessImpl :: BrowserApi -> Effect (Promise Unit)
foreign import ensureBackupAlarmImpl :: BrowserApi -> Effect (Promise Unit)
foreign import clearBackupAlarmImpl :: BrowserApi -> Effect (Promise Unit)
foreign import onBackupAlarmImpl :: BrowserApi -> Effect Unit -> Effect Unit
foreign import backupFilename :: Effect String
foreign import downloadJsonFileImpl :: BrowserApi -> String -> Nullable Boolean -> String -> Effect (Promise Unit)

-- | Activate a tab and focus its window (the FFI resolves the window from the tab).
focusTab :: BrowserApi -> Int -> Aff Unit
focusTab api tabId = toAffE (focusTabImpl api tabId)

createTab :: BrowserApi -> Maybe Int -> Maybe Int -> Maybe String -> Aff Unit
createTab api windowId index url = toAffE (createTabImpl api (toNullable windowId) (toNullable index) (toNullable url))

-- | Open one new browser window populated with the given urls, tagged so the
-- | `windows.onCreated` listener can pair it back to container node `nodeId` (via
-- | a `windowBound` event) instead of the reducer guessing from the pending FIFO.
createWindow :: BrowserApi -> String -> Array String -> Aff Unit
createWindow api nodeId urls = toAffE (createWindowImpl api nodeId urls)

-- | Move a live tab into an existing browser window at `index` (-1 = append).
-- | Used when a live tab is reorganized under a container that is already a live
-- | window.
moveTabToWindow :: BrowserApi -> Int -> Int -> Int -> Aff Unit
moveTabToWindow api tabId windowId index = toAffE (moveTabToWindowImpl api tabId windowId index)

-- | Detach tabs into one brand-new browser window (the first creates it, the rest
-- | move in). Used when live tabs are reorganized under a saved/plain container
-- | (it "goes live") or out to the root.
newWindowWithTabs :: BrowserApi -> Maybe String -> Array Int -> Aff Unit
newWindowWithTabs api nodeId tabIds = toAffE (newWindowWithTabsImpl api (toNullable nodeId) tabIds)

removeTab :: BrowserApi -> Int -> Aff Unit
removeTab api tabId = toAffE (removeTabImpl api tabId)

-- | Open/focus the original-style full-size outliner popup. `sourceWindowId`
-- | distinguishes a click from a docked sidebar (focus an existing popup) from a
-- | click inside a full-size popup (spawn another instance).
openFullSizeOutliner :: BrowserApi -> Maybe Int -> Aff Unit
openFullSizeOutliner api sourceWindowId = toAffE (openFullSizeOutlinerImpl api (toNullable sourceWindowId))

-- | Stamp a live tab with its outliner node id (via `browser.sessions`), so a
-- | restart's re-match can re-bind it by that stable id instead of guessing by url.
-- | Best-effort: a missing API or failed write is swallowed in the FFI.
tagTab :: BrowserApi -> Int -> String -> Aff Unit
tagTab api tabId nodeId = toAffE (tagTabImpl api tabId nodeId)

getAutomaticBackupsEnabled :: BrowserApi -> Aff Boolean
getAutomaticBackupsEnabled api = toAffE (getAutomaticBackupsEnabledImpl api)

setAutomaticBackupsEnabled :: BrowserApi -> Boolean -> Aff Unit
setAutomaticBackupsEnabled api enabled = toAffE (setAutomaticBackupsEnabledImpl api enabled)

automaticBackupDue :: BrowserApi -> Aff Boolean
automaticBackupDue api = toAffE (automaticBackupDueImpl api)

recordAutomaticBackupSuccess :: BrowserApi -> Aff Unit
recordAutomaticBackupSuccess api = toAffE (recordAutomaticBackupSuccessImpl api)

ensureBackupAlarm :: BrowserApi -> Aff Unit
ensureBackupAlarm api = toAffE (ensureBackupAlarmImpl api)

clearBackupAlarm :: BrowserApi -> Aff Unit
clearBackupAlarm api = toAffE (clearBackupAlarmImpl api)

onBackupAlarm :: BrowserApi -> Effect Unit -> Effect Unit
onBackupAlarm = onBackupAlarmImpl

-- | Filename for a manual Export. Undated, unlike `backupFilename`: the browser
-- | uniquifies (`grove(1).json`), so repeated exports never clobber each other.
exportFilename :: String
exportFilename = "grove.json"

-- | Write the daily automatic backup. Never prompts for a location — it runs
-- | unattended, possibly with no sidebar open.
downloadBackupFile :: BrowserApi -> String -> String -> Aff Unit
downloadBackupFile api filename content =
  toAffE (downloadJsonFileImpl api filename (toNullable (Just false)) content)

-- | Write the file for a manual Export. Where it lands is left to the user's own
-- | "Always ask where to save files" setting (the option is omitted rather than
-- | forced), which is how the download behaved before it moved here.
-- |
-- | Both writers live in the background for the same reason: the payload is the
-- | whole tree and must not cross `runtime.sendMessage`.
downloadExportFile :: BrowserApi -> String -> Aff Unit
downloadExportFile api content =
  toAffE (downloadJsonFileImpl api exportFilename (toNullable Nothing) content)
