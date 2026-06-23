-- | The sidebar: a thin Halogen view of the background-owned forest. On open it
-- | pulls one snapshot (retrying until the background answers), then stays live
-- | by applying broadcast patches. It renders only visible rows (O(visible)),
-- | keyed by NodeId, and sends every user action back as a command. The toolbar
-- | adds search (incl. inside collapsed groups), font zoom, and JSON
-- | export/import.
module Sidebar.Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
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
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Model.Codec (Snapshot, decodePatch, decodeSnapshot, encodeSnapshot)
import Model.Command (Command(..), Request(..), encodeRequest)
import Model.PortableImport (portableToSnapshot)
import Model.Tree (applyPatch, searchVisible, visible)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, Status(..), displayTitle, emptyModel)
import Web.UIEvent.KeyboardEvent (key)

foreign import allowDrops :: Effect Unit
foreign import downloadJson :: String -> String -> Effect Unit
foreign import pickJson :: (String -> Effect Unit) -> Effect Unit
foreign import getZoom :: Effect Number
foreign import setZoom :: Number -> Effect Unit

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
  , query :: String
  , zoom :: Number
  , notice :: Maybe String
  , listener :: Maybe (HS.Listener Action)
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
  | SetQuery String
  | Zoom Number
  | ExportClick
  | ImportClick
  | ImportLoaded String
  | ClearNotice

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { api: Nothing, model: emptyModel, editing: Nothing, dragId: Nothing, query: "", zoom: 1.0, notice: Nothing, listener: Nothing }
  , render
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    api <- H.liftEffect getBrowser
    H.liftEffect allowDrops
    z <- H.liftEffect getZoom
    { emitter, listener } <- H.liftEffect HS.create
    H.modify_ _ { api = Just api, zoom = z, listener = Just listener }
    -- subscribe to broadcasts first, so no patch is missed after the snapshot
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

  SetQuery q -> H.modify_ _ { query = q }
  Zoom factor -> do
    st <- H.get
    let z = clamp 0.6 2.5 (st.zoom * factor)
    H.modify_ _ { zoom = z }
    H.liftEffect (setZoom z)
  ExportClick -> do
    st <- H.get
    H.liftEffect (downloadJson "tabs-outliner.json" (stringify (encodeSnapshot st.model)))
  ImportClick -> do
    H.modify_ _ { notice = Nothing }
    st <- H.get
    case st.listener of
      Just l -> H.liftEffect (pickJson (\text -> HS.notify l (ImportLoaded text)))
      Nothing -> pure unit
  ImportLoaded text -> case parseImport text of
    Right snap -> do
      H.modify_ _ { notice = Nothing }
      sendCommand (Import snap)
    Left msg -> H.modify_ _ { notice = Just msg }
  ClearNotice -> H.modify_ _ { notice = Nothing }

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

-- Accept our own flat export OR the original's nested portable-tree export.
parseImport :: String -> Either String Snapshot
parseImport text = case jsonParser text of
  Left _ -> Left "Import failed: that file isn't valid JSON."
  Right json -> case decodeSnapshot json of
    Right snap -> Right snap
    Left _ -> case portableToSnapshot json of
      Just snap -> Right snap
      Nothing -> Left "Import failed: unrecognized format (expected this app's export or a Tab Session Outliner portable tree)."

render :: State -> H.ComponentHTML Action () Aff
render st =
  HH.div [ HP.id "app", HP.style ("--font-scale:" <> show st.zoom) ]
    ( [ HH.div [ HP.id "toolbar" ]
          [ HH.input
              [ HP.id "search", HP.placeholder "Search", HP.value st.query, HE.onValueInput SetQuery ]
          , tbtn "zoom-out" "A-" (Zoom (1.0 / 1.1))
          , tbtn "zoom-in" "A+" (Zoom 1.1)
          , tbtn "new-group" "New group" NewGroupTop
          , tbtn "export" "Export" ExportClick
          , tbtn "import" "Import" ImportClick
          ]
      ]
        <> noticeBanner st.notice
        <> [ HK.div [ HP.id "tree", HP.attr (AttrName "role") "tree" ] (map (row st) entries) ]
    )
  where
  entries = if st.query == "" then visible st.model else searchVisible st.query st.model
  tbtn i label act = HH.button [ HP.id i, HE.onClick \_ -> act ] [ HH.text label ]

noticeBanner :: Maybe String -> Array (H.ComponentHTML Action () Aff)
noticeBanner = case _ of
  Nothing -> []
  Just msg -> [ HH.div [ HP.id "notice", HE.onClick \_ -> ClearNotice ] [ HH.text (msg <> "   ✕") ] ]

row :: State -> { id :: NodeId, depth :: Int } -> Tuple String (H.ComponentHTML Action () Aff)
row st { id, depth } = Tuple id $ case Map.lookup id st.model.nodes of
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
