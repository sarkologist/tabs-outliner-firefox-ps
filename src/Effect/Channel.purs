-- | The entire sidebar<->background sync layer, over `browser.runtime`
-- | messaging. Three operations, deliberately not a projection protocol:
-- |   - sidebar `request` (e.g. getSnapshot) -> background `onRequest` replies
-- |   - background `broadcast` (a patch) -> sidebar `onBroadcast` applies
-- | Messages are argonaut Json (plain JS values), routed by a `kind` tag so a
-- | context ignores messages meant for the other role.
module Effect.Channel
  ( request
  , onRequest
  , broadcast
  , onBroadcast
  ) where

import Prelude

import Control.Promise (Promise, fromAff, toAffE)
import Data.Argonaut.Core (Json)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Browser (BrowserApi)

foreign import requestImpl :: BrowserApi -> Json -> Effect (Promise Json)
foreign import onRequestImpl :: BrowserApi -> (Json -> Effect (Promise Json)) -> Effect Unit
foreign import broadcastImpl :: BrowserApi -> Json -> Effect Unit
foreign import onBroadcastImpl :: BrowserApi -> (Json -> Effect Unit) -> Effect Unit

request :: BrowserApi -> Json -> Aff Json
request api j = toAffE (requestImpl api j)

onRequest :: BrowserApi -> (Json -> Aff Json) -> Effect Unit
onRequest api handler = onRequestImpl api (\j -> fromAff (handler j))

broadcast :: BrowserApi -> Json -> Effect Unit
broadcast = broadcastImpl

onBroadcast :: BrowserApi -> (Json -> Effect Unit) -> Effect Unit
onBroadcast = onBroadcastImpl
