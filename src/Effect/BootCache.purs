-- | The sidebar caches the top window it last rendered (in shared localStorage),
-- | so a fresh open — especially against a *suspended* background event page,
-- | which otherwise takes ~half a second to wake and reload the whole model —
-- | can paint instantly from the cache while the live window loads behind it.
module Effect.BootCache (save, load) where

import Prelude (Unit)
import Effect (Effect)

-- | Persist the top-window JSON (the raw GetView response).
foreign import save :: String -> Effect Unit

-- | The last cached top window ("" when none / unavailable).
foreign import load :: Effect String
