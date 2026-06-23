-- | The WebExtensions `commands` API, scoped to the one browser-level command we
-- | ship: `_execute_sidebar_action` (toggle the sidebar). This is separate from
-- | the in-page shortcuts in Effect.Settings — a browser command must work even
-- | when the sidebar is closed, so the browser owns it, and we read/write it via
-- | commands.getAll / commands.update / commands.reset. All calls degrade
-- | gracefully when the API is absent (e.g. the test harness), so the options
-- | page can fall back to a "configure it in Firefox" note.
module Effect.Commands
  ( isMac
  , getSidebarToggle
  , setSidebarToggle
  , resetSidebarToggle
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)

-- | Whether the platform is macOS (so the options page can record ⌘ as Command).
foreign import isMac :: Effect Boolean

foreign import getSidebarToggleImpl :: Effect (Promise (Nullable String))
foreign import setSidebarToggleImpl :: String -> Effect (Promise (Nullable String))
foreign import resetSidebarToggleImpl :: Effect (Promise (Nullable String))

-- | The current sidebar-toggle shortcut: Nothing when the commands API is
-- | unavailable, Just "" when the command exists but no key is bound.
getSidebarToggle :: Aff (Maybe String)
getSidebarToggle = toMaybe <$> toAffE getSidebarToggleImpl

-- | Set the sidebar-toggle shortcut. Nothing on success; Just msg if the browser
-- | rejected the shortcut (the message is the browser's own).
setSidebarToggle :: String -> Aff (Maybe String)
setSidebarToggle s = toMaybe <$> toAffE (setSidebarToggleImpl s)

-- | Reset it to the manifest default. Nothing on success (or when unavailable).
resetSidebarToggle :: Aff (Maybe String)
resetSidebarToggle = toMaybe <$> toAffE resetSidebarToggleImpl
