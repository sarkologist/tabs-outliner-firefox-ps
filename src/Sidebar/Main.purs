-- | The sidebar: a thin Halogen view of the background-owned forest. On open it
-- | pulls one snapshot (retrying until the background answers), then stays live
-- | by applying broadcast patches. It renders only visible rows (O(visible)),
-- | keyed by NodeId, and sends user actions back as commands.
module Sidebar.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay)
import Effect.Browser (BrowserApi, getBrowser)
import Effect.Channel (onBroadcast, request)
import Effect.Persist (modelFromLoaded)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Model.Codec (Snapshot, decodePatch, decodeSnapshot)
import Model.Command (Command(..), Request(..), encodeRequest)
import Model.Tree (applyPatch, visible)
import Model.Types (Model, Node, NodeId, Patch, Status(..), displayTitle, emptyModel)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI component unit body

type State = { api :: Maybe BrowserApi, model :: Model }

data Action
  = Initialize
  | GotPatch Patch
  | Toggle NodeId Boolean

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { api: Nothing, model: emptyModel }
  , render
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    api <- H.liftEffect getBrowser
    H.modify_ _ { api = Just api }
    -- subscribe to broadcasts first, so no patch is missed after the snapshot
    { emitter, listener } <- H.liftEffect HS.create
    H.liftEffect $ onBroadcast api \json -> case decodePatch json of
      Right p -> HS.notify listener (GotPatch p)
      Left _ -> pure unit
    void $ H.subscribe emitter
    -- pull the initial snapshot (retry until the background is up)
    msnap <- H.liftAff (requestSnapshot api 40)
    case msnap of
      Just snap -> H.modify_ _ { model = modelFromLoaded snap.nodes snap.roots }
      Nothing -> pure unit
  GotPatch p -> H.modify_ \s -> s { model = applyPatch p s.model }
  Toggle nid value -> sendCommand (Collapse nid value)

sendCommand :: forall o. Command -> H.HalogenM State Action () o Aff Unit
sendCommand c = do
  st <- H.get
  case st.api of
    Just api -> void $ H.liftAff (attempt (request api (encodeRequest (RunCommand c))))
    Nothing -> pure unit

requestSnapshot :: BrowserApi -> Int -> Aff (Maybe Snapshot)
requestSnapshot _ 0 = pure Nothing
requestSnapshot api n = do
  r <- attempt (request api (encodeRequest GetSnapshot))
  case r of
    Right json | Right snap <- decodeSnapshot json -> pure (Just snap)
    _ -> do
      delay (Milliseconds 50.0)
      requestSnapshot api (n - 1)

render :: State -> H.ComponentHTML Action () Aff
render st =
  HK.div
    [ HP.id "tree", HP.attr (AttrName "role") "tree" ]
    (map row (visible st.model))
  where
  row { id, depth } = Tuple id $ case Map.lookup id st.model.nodes of
    Nothing -> HH.text ""
    Just n -> renderNode depth n

renderNode :: Int -> Node -> H.ComponentHTML Action () Aff
renderNode depth n =
  HH.div
    [ HP.classes (map ClassName [ "row", statusClass n.status ])
    , HP.attr (AttrName "data-node-id") n.id
    , HP.attr (AttrName "data-status") (statusClass n.status)
    , HP.attr (AttrName "role") "treeitem"
    , HP.style ("padding-left:" <> show (4 + depth * 14) <> "px")
    ]
    [ toggleEl n
    , HH.span [ HP.class_ (ClassName "title") ] [ HH.text (displayTitle n) ]
    ]

toggleEl :: Node -> H.ComponentHTML Action () Aff
toggleEl n
  | Array.null n.children = HH.span [ HP.class_ (ClassName "spacer") ] [ HH.text "" ]
  | otherwise = HH.span
      [ HP.class_ (ClassName "toggle"), HE.onClick \_ -> Toggle n.id (not n.collapsed) ]
      [ HH.text (if n.collapsed then "▸" else "▾") ]

statusClass :: Status -> String
statusClass Live = "live"
statusClass Closed = "closed"
