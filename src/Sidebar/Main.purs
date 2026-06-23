-- | The sidebar: a thin Halogen view of the background-owned forest. On open it
-- | pulls one snapshot (retrying until the background answers), then stays live
-- | by applying broadcast patches. It renders only visible rows (O(visible)),
-- | keyed by NodeId, and sends every user action back as a command.
module Sidebar.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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
import Model.Types (Kind(..), Model, Node, NodeId, Patch, Status(..), displayTitle, emptyModel)
import Web.UIEvent.KeyboardEvent (key)

foreign import allowDrops :: Effect Unit

main :: Effect Unit
main = HA.runHalogenAff do
  bodyEl <- HA.awaitBody
  void $ runUI component unit bodyEl

type Editing = { id :: NodeId, text :: String }

type State =
  { api :: Maybe BrowserApi
  , model :: Model
  , editing :: Maybe Editing
  , dragId :: Maybe NodeId
  }

data Action
  = Initialize
  | GotPatch Patch
  | Toggle NodeId Boolean
  | ClickRow NodeId
  | CloseClick NodeId
  | DeleteClick NodeId
  | FlattenClick NodeId
  | NewGroupTop
  | StartRename NodeId String
  | EditInput String
  | EditKey String
  | CommitRename
  | CancelRename
  | DragStart NodeId
  | DropOn NodeId
  | DragEnd

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { api: Nothing, model: emptyModel, editing: Nothing, dragId: Nothing }
  , render
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    api <- H.liftEffect getBrowser
    H.liftEffect allowDrops
    H.modify_ _ { api = Just api }
    -- subscribe to broadcasts first, so no patch is missed after the snapshot
    { emitter, listener } <- H.liftEffect HS.create
    H.liftEffect $ onBroadcast api \json -> case decodePatch json of
      Right p -> HS.notify listener (GotPatch p)
      Left _ -> pure unit
    void $ H.subscribe emitter
    msnap <- H.liftAff (requestSnapshot api 40)
    case msnap of
      Just snap -> H.modify_ _ { model = modelFromLoaded snap.nodes snap.roots }
      Nothing -> pure unit

  GotPatch p -> H.modify_ \s -> s { model = applyPatch p s.model }
  Toggle nid value -> sendCommand (Collapse nid value)
  ClickRow nid -> sendCommand (Activate nid)
  CloseClick nid -> sendCommand (CloseNode nid)
  DeleteClick nid -> sendCommand (Delete nid)
  FlattenClick nid -> sendCommand (Flatten nid)
  NewGroupTop -> sendCommand (NewGroup Nothing 0)

  StartRename nid text -> H.modify_ _ { editing = Just { id: nid, text } }
  EditInput text -> H.modify_ \s -> s { editing = map (_ { text = text }) s.editing }
  EditKey k
    | k == "Enter" -> handleAction CommitRename
    | k == "Escape" -> handleAction CancelRename
    | otherwise -> pure unit
  CommitRename -> do
    st <- H.get
    case st.editing of
      Just e | e.text /= "" -> sendCommand (Rename e.id e.text)
      _ -> pure unit
    H.modify_ _ { editing = Nothing }
  CancelRename -> H.modify_ _ { editing = Nothing }

  DragStart nid -> H.modify_ _ { dragId = Just nid }
  DragEnd -> H.modify_ _ { dragId = Nothing }
  DropOn targetId -> do
    st <- H.get
    case st.dragId of
      Just dragId | dragId /= targetId -> case Map.lookup targetId st.model.nodes of
        Just target -> sendCommand (dropCommand dragId target st.model)
        Nothing -> pure unit
      _ -> pure unit
    H.modify_ _ { dragId = Nothing }

-- Dropping onto a group nests (append); onto anything else places before it as a
-- sibling. Enough to express reorder and re-parent; finer drop zones are future.
dropCommand :: NodeId -> Node -> Model -> Command
dropCommand dragId target model = case target.kind of
  KGroup -> Move dragId (Just target.id) (Array.length target.children)
  _ -> Move dragId target.parent (indexOf target.id target.parent model)

indexOf :: NodeId -> Maybe NodeId -> Model -> Int
indexOf tid mParent model = fromMaybe 0 (Array.elemIndex tid siblings)
  where
  siblings = case mParent of
    Just pid -> fromMaybe [] (_.children <$> Map.lookup pid model.nodes)
    Nothing -> model.roots

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
  HH.div [ HP.id "app" ]
    [ HH.div [ HP.id "toolbar" ]
        [ HH.button [ HP.id "new-group", HE.onClick \_ -> NewGroupTop ] [ HH.text "New group" ] ]
    , HK.div [ HP.id "tree", HP.attr (AttrName "role") "tree" ] (map row (visible st.model))
    ]
  where
  row { id, depth } = Tuple id $ case Map.lookup id st.model.nodes of
    Nothing -> HH.text ""
    Just n -> renderNode st depth n

renderNode :: State -> Int -> Node -> H.ComponentHTML Action () Aff
renderNode st depth n =
  HH.div
    [ HP.classes (map ClassName [ "row", statusClass n.status ])
    , HP.attr (AttrName "data-node-id") n.id
    , HP.attr (AttrName "data-status") (statusClass n.status)
    , HP.attr (AttrName "role") "treeitem"
    , HP.style ("padding-left:" <> show (4 + depth * 14) <> "px")
    , HP.draggable true
    , HE.onDragStart \_ -> DragStart n.id
    , HE.onDrop \_ -> DropOn n.id
    , HE.onDragEnd \_ -> DragEnd
    ]
    ([ toggleEl n, body st n ] <> buttons n)

body :: State -> Node -> H.ComponentHTML Action () Aff
body st n = case st.editing of
  Just e | e.id == n.id ->
    HH.input
      [ HP.class_ (ClassName "rename-input")
      , HP.value e.text
      , HE.onValueInput EditInput
      , HE.onKeyDown (EditKey <<< key)
      , HE.onBlur \_ -> CommitRename
      ]
  _ ->
    HH.span
      [ HP.class_ (ClassName "title"), HE.onClick \_ -> ClickRow n.id ]
      [ HH.text (displayTitle n) ]

buttons :: Node -> Array (H.ComponentHTML Action () Aff)
buttons n =
  [ btn "btn-rename" "✎" (StartRename n.id (displayTitle n)) ]
    <> (if n.status == Live then [ btn "btn-close" "⊗" (CloseClick n.id) ] else [])
    <> (if n.kind == KGroup then [ btn "btn-flatten" "⇲" (FlattenClick n.id) ] else [])
    <> [ btn "btn-delete" "✕" (DeleteClick n.id) ]
  where
  btn cls label act = HH.button [ HP.class_ (ClassName cls), HE.onClick \_ -> act ] [ HH.text label ]

toggleEl :: Node -> H.ComponentHTML Action () Aff
toggleEl n
  | Array.null n.children = HH.span [ HP.class_ (ClassName "spacer") ] [ HH.text "" ]
  | otherwise = HH.span
      [ HP.class_ (ClassName "toggle"), HE.onClick \_ -> Toggle n.id (not n.collapsed) ]
      [ HH.text (if n.collapsed then "▸" else "▾") ]

statusClass :: Status -> String
statusClass Live = "live"
statusClass Closed = "closed"
