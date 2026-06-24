-- | The sidebar: a thin Halogen view of the background-owned forest. On open it
-- | pulls one snapshot (retrying until the background answers), then stays live
-- | by applying broadcast patches. It renders only the rows in the viewport
-- | (virtualized — O(viewport), independent of how many are visible/expanded),
-- | keyed by NodeId, and sends every user action back as a command. The toolbar
-- | adds search (incl. inside collapsed groups), font zoom, and JSON
-- | export/import.
module Sidebar.Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Int.Bits (and, or)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Browser (BrowserApi, getBrowser)
import Effect.Channel (onBroadcast, request)
import Effect.Persist as Persist
import Effect.Settings as Settings
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Model.Codec (Snapshot, decodePatch, decodeSnapshot, encodeSnapshot)
import Model.Command (Command(..), Request(..), encodeRequest)
import Model.PortableImport (portableToSnapshot)
import Model.Shortcuts as Sh
import Model.Tree (Entry, applyPatch, searchVisible, visible)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, Status(..), displayTitle, emptyModel)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (key)

foreign import allowDrops :: Effect Unit
foreign import downloadJson :: String -> String -> Effect Unit
foreign import pickJson :: (String -> Effect Unit) -> Effect Unit
foreign import getZoom :: Effect Number
foreign import setZoom :: Number -> Effect Unit
foreign import scrollMetrics :: Event -> { top :: Number, height :: Number }
foreign import treeViewportHeight :: Effect Number
foreign import onResize :: Effect Unit -> Effect Unit
foreign import focusSearch :: Effect Unit
foreign import openOptions :: Effect Unit

-- height of one row in px at zoom 1. Set as each row's inline `height`, so this
-- is the source of truth for row height (the CSS centers content within it).
-- Matches the original's compact `--node-row-height: 18px`.
baseRowHeight :: Number
baseRowHeight = 18.0

-- extra rows rendered above/below the viewport for smooth scrolling
overscan :: Int
overscan = 6

-- keep zoom in a sane range (also guards a corrupt persisted value: rowH must be > 0)
clampZoom :: Number -> Number
clampZoom = clamp 0.6 2.5

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
  , hover :: Maybe NodeId
  , query :: String
  , zoom :: Number
  , notice :: Maybe String
  , scrollTop :: Number
  , viewportH :: Number
  , listener :: Maybe (HS.Listener Action)
  }

data Action
  = Initialize
  | GotPatch Patch
  | Scrolled { top :: Number, height :: Number }
  | Remeasure
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
  | SetHover (Maybe NodeId)
  | SetQuery String
  | Zoom Number
  | ExportClick
  | ImportClick
  | ImportLoaded String
  | ClearNotice
  | OpenOptions
  | RunUndo
  | RunRedo
  | RunShortcut Sh.Cmd

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { api: Nothing, model: emptyModel, editing: Nothing, dragId: Nothing, hover: Nothing, query: "", zoom: 1.0, notice: Nothing, scrollTop: 0.0, viewportH: 600.0, listener: Nothing }
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
    H.modify_ _ { api = Just api, zoom = clampZoom z, listener = Just listener }
    -- subscribe to broadcasts first, so no patch is missed after the snapshot
    H.liftEffect $ onBroadcast api \json -> case decodePatch json of
      Right p -> HS.notify listener (GotPatch p)
      Left _ -> pure unit
    void $ H.subscribe emitter
    H.liftEffect $ onResize (HS.notify listener Remeasure)
    -- Global keyboard shortcuts: on each keydown read the (possibly user-edited)
    -- bindings and dispatch the matching command. Reading per-press keeps the
    -- sidebar in sync with the options page with no reload.
    H.liftEffect $ Settings.onShortcut \combo -> case editorShortcut combo of
      Just act -> do
        HS.notify listener act
        pure true
      Nothing -> do
        overrides <- Settings.getShortcuts
        case Sh.cmdForCombo overrides combo of
          Just cmd -> do
            HS.notify listener (RunShortcut cmd)
            pure true
          Nothing -> pure false
    handleAction Remeasure
    -- Load the initial model straight from IndexedDB (same extension origin as
    -- the background, which is its only writer). This skips an O(total) snapshot
    -- encode + structured-clone over the message channel — the slow part of
    -- opening the sidebar on a large tree. Live updates still arrive as patches,
    -- and we subscribed above, so none are missed during the load.
    db <- H.liftAff Persist.open
    loaded <- H.liftAff (Persist.load db)
    H.modify_ _ { model = Persist.modelFromLoaded loaded.nodes loaded.roots }

  GotPatch p -> H.modify_ \s -> s { model = applyPatch p s.model }
  Scrolled m -> H.modify_ _ { scrollTop = m.top, viewportH = m.height }
  Remeasure -> do
    h <- H.liftEffect treeViewportHeight
    H.modify_ _ { viewportH = h }
  Toggle nid value -> sendCommand (Collapse nid value)
  ClickRow nid -> sendCommand (Activate nid)
  CloseClick nid -> sendCommand (CloseNode nid)
  DeleteClick nid -> sendCommand (Delete nid)
  FlattenClick nid -> sendCommand (Flatten nid)
  NewGroupTop -> sendCommand (NewGroup Nothing 0)

  StartRename nid text -> H.modify_ _ { editing = Just { id: nid, text }, hover = Nothing }
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

  DragStart nid -> H.modify_ _ { dragId = Just nid, hover = Nothing }
  DragEnd -> H.modify_ _ { dragId = Nothing }
  -- Track the hovered row to drive the subtree guide lines. Ignore while
  -- renaming so a stray pointer move can't re-render the focused input out from
  -- under the user; while dragging there is no guide (dragId set, hover cleared).
  SetHover mh -> H.modify_ \s -> case s.editing of
    Just _ -> s
    Nothing -> s { hover = mh }
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
    let z = clampZoom (st.zoom * factor)
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
  OpenOptions -> H.liftEffect openOptions
  RunUndo -> sendRequest Undo
  RunRedo -> sendRequest Redo
  RunShortcut cmd -> case cmd of
    Sh.NewGroup -> handleAction NewGroupTop
    Sh.FocusSearch -> H.liftEffect focusSearch
    Sh.ZoomIn -> handleAction (Zoom 1.1)
    Sh.ZoomOut -> handleAction (Zoom (1.0 / 1.1))
    Sh.ResetZoom -> do
      H.modify_ _ { zoom = 1.0 }
      H.liftEffect (setZoom 1.0)
    Sh.Export -> handleAction ExportClick
    Sh.Import -> handleAction ImportClick

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
sendCommand = sendRequest <<< RunCommand

-- | Fire a request at the background and forget the reply: the model updates
-- | when the resulting patch broadcasts back, exactly as for a command.
sendRequest :: forall o. Request -> H.HalogenM State Action () o Aff Unit
sendRequest req = do
  st <- H.get
  case st.api of
    Just api -> void $ H.liftAff (attempt (request api (encodeRequest req)))
    Nothing -> pure unit

-- | Fixed (non-rebindable) editor shortcuts, matched before the configurable
-- | toolbar ones. Undo/redo need a Ctrl/Cmd modifier and are universal enough not
-- | to belong on the options page. Combos are the canonical strings Effect.Settings
-- | builds (modifier order Ctrl, Alt, Shift, Meta), so Cmd+Shift+Z is "Shift+Meta+z".
-- | We accept Ctrl and Meta (mac), plus Ctrl/Cmd+Y as the common Windows redo.
editorShortcut :: String -> Maybe Action
editorShortcut = case _ of
  "Ctrl+z" -> Just RunUndo
  "Meta+z" -> Just RunUndo
  "Ctrl+Shift+z" -> Just RunRedo
  "Shift+Meta+z" -> Just RunRedo
  "Ctrl+y" -> Just RunRedo
  "Meta+y" -> Just RunRedo
  _ -> Nothing

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
          , iconBtn "undo" "Undo (Ctrl+Z)" "undo" RunUndo
          , iconBtn "redo" "Redo (Ctrl+Shift+Z)" "redo" RunRedo
          , textBtn "zoom-out" "Zoom out" "A−" (Zoom (1.0 / 1.1))
          , textBtn "zoom-in" "Zoom in" "A+" (Zoom 1.1)
          , iconBtn "new-group" "New group" "group" NewGroupTop
          , iconBtn "export" "Export" "export" ExportClick
          , iconBtn "import" "Import" "import" ImportClick
          , iconBtn "options" "Options" "gear" OpenOptions
          ]
      ]
        <> noticeBanner st.notice
        <>
          [ HH.div
              [ HP.id "tree"
              , HP.attr (AttrName "role") "tree"
              , HE.onScroll (Scrolled <<< scrollMetrics)
              , HE.onMouseLeave \_ -> SetHover Nothing
              ]
              [ HK.div
                  [ HP.id "tree-inner", HP.style ("position:relative;height:" <> show totalH <> "px") ]
                  (Array.mapWithIndex slot slice)
              ]
          ]
    )
  where
  entries = if st.query == "" then visible st.model else searchVisible st.query st.model
  n = Array.length entries
  rowH = baseRowHeight * st.zoom
  totalH = Int.toNumber n * rowH
  count = Int.ceil (st.viewportH / rowH) + overscan * 2
  -- clamp the window so a shrunk list (collapse/delete/zoom) shows its tail
  -- rather than going blank until the browser fires a corrective scroll
  firstIdx = clamp 0 (max 0 (n - count)) (Int.floor (st.scrollTop / rowH) - overscan)
  slice = Array.slice firstIdx (firstIdx + count) entries
  -- subtree guide lines for the hovered row (empty when not hovering / renaming)
  guide = case st.hover, st.editing of
    Just h, Nothing -> case Array.findIndex (\e -> e.id == h) entries of
      Just hi -> buildGuide entries hi
      Nothing -> emptyGuide
    _, _ -> emptyGuide
  slot i entry = Tuple entry.id $ case Map.lookup entry.id st.model.nodes of
    Nothing -> HH.text ""
    Just node -> renderNode st.editing guide (firstIdx + i) entry.depth rowH node
  iconBtn i label name act =
    HH.button
      [ HP.id i, HP.title label, HP.attr (AttrName "aria-label") label, HE.onClick \_ -> act ]
      [ toolbarIcon name ]
  textBtn i label glyph act =
    HH.button [ HP.id i, HP.title label, HE.onClick \_ -> act ] [ HH.text glyph ]

noticeBanner :: Maybe String -> Array (H.ComponentHTML Action () Aff)
noticeBanner = case _ of
  Nothing -> []
  Just msg -> [ HH.div [ HP.id "notice", HE.onClick \_ -> ClearNotice ] [ HH.text (msg <> "   ✕") ] ]

renderNode :: Maybe Editing -> Guide -> Int -> Int -> Number -> Node -> H.ComponentHTML Action () Aff
renderNode editing guide idx depth rowH n =
  HH.div
    [ HP.classes (map ClassName (rowClasses n))
    , HP.attr (AttrName "data-node-id") n.id
    , HP.attr (AttrName "data-status") (statusClass n.status)
    , HP.attr (AttrName "role") "treeitem"
    , HP.style
        ( "position:absolute;left:0;right:0;height:" <> show rowH
            <> "px;top:" <> show (Int.toNumber idx * rowH)
            <> "px;--depth:" <> show depth
        )
    , HP.draggable true
    , HE.onMouseEnter \_ -> SetHover (Just n.id)
    , HE.onDragStart \_ -> DragStart n.id
    , HE.onDrop \_ -> DropOn n.id
    , HE.onDragEnd \_ -> DragEnd
    ]
    -- The guide layer is ALWAYS present (just empty when this row has no guide)
    -- and last. That keeps every row's child list structurally identical across
    -- hovers: hovering only mutates the guide layer's own children (the lines),
    -- never inserts/removes a sibling of the toggle/title/actions. Otherwise the
    -- DOM mutation races a pointer hit-test on the hover-revealed action buttons.
    -- It paints behind the content via z-index, not DOM order.
    [ toggleEl n, body editing n, actionsEl n, guideLayer guide idx ]

rowClasses :: Node -> Array String
rowClasses n =
  [ "row", statusClass n.status, kindClass n.kind ]
    <> (if n.active && n.status == Live then [ "active" ] else [])

body :: Maybe Editing -> Node -> H.ComponentHTML Action () Aff
body editing n = case editing of
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

-- Hover-revealed action cluster (rename / close / flatten / delete).
actionsEl :: Node -> H.ComponentHTML Action () Aff
actionsEl n = HH.span [ HP.class_ (ClassName "node-actions") ] (buttons n)

buttons :: Node -> Array (H.ComponentHTML Action () Aff)
buttons n =
  [ btn "btn-rename" "Rename" "pencil" (StartRename n.id (displayTitle n)) ]
    <> (if n.status == Live then [ btn "btn-close" "Close" "close-circle" (CloseClick n.id) ] else [])
    <> (if n.kind == KGroup then [ btn "btn-flatten" "Flatten" "flatten" (FlattenClick n.id) ] else [])
    <> [ btn "btn-delete" "Delete" "trash" (DeleteClick n.id) ]
  where
  btn cls label name act =
    HH.button
      [ HP.class_ (ClassName cls), HP.title label, HP.attr (AttrName "aria-label") label, HE.onClick \_ -> act ]
      [ icon name ]

toggleEl :: Node -> H.ComponentHTML Action () Aff
toggleEl n
  | Array.null n.children = HH.span [ HP.class_ (ClassName "spacer") ] []
  | otherwise = HH.span
      [ HP.class_ (ClassName "toggle"), HE.onClick \_ -> Toggle n.id (not n.collapsed) ]
      [ icon (if n.collapsed then "chevron-right" else "chevron-down") ]

statusClass :: Status -> String
statusClass Live = "live"
statusClass Closed = "closed"

kindClass :: Kind -> String
kindClass KWindow = "kind-window"
kindClass KTab = "kind-tab"
kindClass KGroup = "kind-group"

-- Icons -----------------------------------------------------------------------

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

-- An icon from the sprite in sidebar.html: <svg class=…><use href="#icon-NAME"/></svg>.
-- The class is set with setAttribute (HP.attr), not HP.classes: Halogen's
-- HP.classes writes the `className` *property*, which is a read-only
-- SVGAnimatedString on SVG elements, so it silently no-ops (leaving the icon
-- unstyled at its default 300×150 size). aria-hidden/href are attributes too.
iconWith :: forall w i. Array String -> String -> HH.HTML w i
iconWith classes name =
  HH.elementNS svgNS (ElemName "svg")
    [ HP.attr (AttrName "class") (joinWith " " classes), HP.attr (AttrName "aria-hidden") "true" ]
    [ HH.elementNS svgNS (ElemName "use") [ HP.attr (AttrName "href") ("#icon-" <> name) ] [] ]

icon :: forall w i. String -> HH.HTML w i
icon = iconWith [ "button-icon" ]

toolbarIcon :: forall w i. String -> HH.HTML w i
toolbarIcon = iconWith [ "button-icon", "toolbar-icon" ]

-- Subtree guide geometry ------------------------------------------------------

-- | Per-row guide overlay: vertical line segments (depth → segment-flag bitset)
-- | and an optional horizontal connector depth, both keyed by a row's flat
-- | preorder index.
type Guide = { verticals :: Map Int (Map Int Int), horizontals :: Map Int Int }

emptyGuide :: Guide
emptyGuide = { verticals: Map.empty, horizontals: Map.empty }

-- which half (or both) of a row a vertical line spans
guideTop :: Int
guideTop = 1

guideBottom :: Int
guideBottom = 2

guideFull :: Int
guideFull = 3

-- Cap the subtree size we draw guides for, so hovering a huge expanded group
-- doesn't spray thousands of line elements (mirrors the original's guard).
maxGuideRows :: Int
maxGuideRows = 1000

-- | Build the guide for the row hovered at flat preorder index `h`: a vertical
-- | line connecting the node up to its parent, vertical connectors threading
-- | down each level of its subtree, and a horizontal stub into every visible
-- | descendant. Pure over the `entries` list; O(subtree) (skipped past the cap).
buildGuide :: Array Entry -> Int -> Guide
buildGuide entries h = case Array.index entries h of
  Nothing -> emptyGuide
  Just target ->
    let
      end = subtreeEnd entries target.depth (h + 1)
    in
      if end - h > maxGuideRows then emptyGuide
      else
        let
          hParent = parentOf entries h target.depth
          acc = foldl (step hParent) { open: Map.empty, verticals: Map.empty, horizontals: Map.empty }
            (Array.range h (end - 1))
        in
          { verticals: acc.verticals, horizontals: acc.horizontals }
  where
  step hParent acc c = case Array.index entries c of
    Nothing -> acc
    Just e ->
      let
        parent = if c == h then hParent else Map.lookup (e.depth - 1) acc.open
        open' = Map.insert e.depth c acc.open
        horizontals' = if e.depth > 0 then Map.insert c e.depth acc.horizontals else acc.horizontals
        verticals' = case parent of
          Just p | e.depth > 0 ->
            let
              v1 = addSeg acc.verticals p e.depth guideBottom
              v2 = addSeg v1 c e.depth guideTop
            in
              if c - p >= 2 then foldl (\a m -> addSeg a m e.depth guideFull) v2 (Array.range (p + 1) (c - 1))
              else v2
          _ -> acc.verticals
      in
        { open: open', verticals: verticals', horizontals: horizontals' }

-- exclusive end of the subtree whose root has depth `d`, scanning from `i`
subtreeEnd :: Array Entry -> Int -> Int -> Int
subtreeEnd entries d i = case Array.index entries i of
  Just e | e.depth > d -> subtreeEnd entries d (i + 1)
  _ -> i

-- index of the parent row (nearest earlier row one level shallower), if any
parentOf :: Array Entry -> Int -> Int -> Maybe Int
parentOf entries h d
  | d <= 0 = Nothing
  | otherwise = go (h - 1)
      where
      go i
        | i < 0 = Nothing
        | otherwise = case Array.index entries i of
            Just e | e.depth < d -> Just i
            Just _ -> go (i - 1)
            Nothing -> Nothing

-- OR a vertical segment flag into (row → depth → flags)
addSeg :: Map Int (Map Int Int) -> Int -> Int -> Int -> Map Int (Map Int Int)
addSeg acc row depth seg =
  Map.insertWith (Map.unionWith or) row (Map.singleton depth seg) acc

-- The always-present absolute overlay for one row; its children (the lines) are
-- empty unless this row participates in the hovered subtree's guide.
guideLayer :: Guide -> Int -> H.ComponentHTML Action () Aff
guideLayer guide idx =
  HH.span [ HP.class_ (ClassName "guide-layer"), HP.attr (AttrName "aria-hidden") "true" ]
    (map vline verts <> horizLine horiz)
  where
  verts = fromMaybe [] (Map.toUnfoldable <$> Map.lookup idx guide.verticals) :: Array (Tuple Int Int)
  horiz = Map.lookup idx guide.horizontals
  vline (Tuple depth flags) =
    HH.span
      [ HP.classes (map ClassName [ "guide-line", "guide-vertical" ])
      , HP.style
          ( "--guide-depth:" <> show depth
              <> ";top:" <> half (flags `and` guideTop)
              <> ";bottom:" <> half (flags `and` guideBottom)
          )
      ]
      []
  half bit = if bit /= 0 then "0" else "50%"
  horizLine = case _ of
    Nothing -> []
    Just depth ->
      [ HH.span
          [ HP.classes (map ClassName [ "guide-line", "guide-horizontal" ])
          , HP.style ("--guide-depth:" <> show depth)
          ]
          []
      ]
