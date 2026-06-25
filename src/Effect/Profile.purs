-- | Opt-in profiler for the sidebar-open path (and anywhere else worth timing),
-- | gated by a localStorage flag the options page toggles. `record` is a no-op
-- | when disabled, so normal use pays only one flag read at boot. Timings land in
-- | a per-page buffer (also on `globalThis.__tabsOutlinerProfile`); `finishBoot`
-- | persists the session for the options page to display and export.
module Effect.Profile
  ( getEnabled
  , setEnabled
  , nowMs
  , clearBuffer
  , record
  , finishBoot
  , readLast
  , clearLast
  , downloadProfile
  ) where

import Prelude (Unit)
import Effect (Effect)

foreign import getEnabled :: Effect Boolean
foreign import setEnabled :: Boolean -> Effect Unit
foreign import nowMs :: Effect Number
foreign import clearBuffer :: Effect Unit
foreign import record :: String -> Number -> Effect Unit
foreign import finishBoot :: String -> Effect Unit
foreign import readLast :: Effect String
foreign import clearLast :: Effect Unit
foreign import downloadProfile :: Effect Unit
