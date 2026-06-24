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
import Data.Int as Int
import Data.Int.Bits (and)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Browser (BrowserApi, getBrowser, getCurrentWindowId)
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
import Model.Drop (dropCommand, dropPlacement)
import Model.Guide (Guide, buildGuide, emptyGuide, guideBottom, guideTop)
import Model.PortableImport (portableToSnapshot)
import Model.Scroll as Scroll
import Model.Shortcuts as Sh
import Model.Tree (applyPatch, searchVisible, visible)
import Model.Types (Kind(..), Model, Node, NodeId, Patch, displayTitle, emptyModel, isLive)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (key)

foreign import allowDrops :: Effect Unit
foreign import downloadJson :: String -> String -> Effect Unit
foreign import pickJson :: (String -> Effect Unit) -> Effect Unit
foreign import getZoom :: Effect Number
foreign import setZoom :: Number -> Effect Unit
foreign import scrollMetrics :: Event -> { top :: Number, height :: Number }
foreign import treeViewportHeight :: Effect Number
foreign import scrollTreeTo :: Number -> Effect Unit
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
  , dropTarget :: Maybe NodeId
  , hover :: Maybe NodeId
  , query :: String
  , zoom :: Number
  , notice :: Maybe String
  , scrollTop :: Number
  , viewportH :: Number
  , listener :: Maybe (HS.Listener Action)
  -- the browser window hosting this sidebar; scopes the scroll-to-active-tab
  , myWindow :: Maybe Int
  -- the active-tab node we last scrolled to, so we follow focus changes without
  -- re-scrolling on every unrelated patch (or fighting the user's own scroll)
  , focusObserved :: Maybe NodeId
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
  | MoveTopLevelClick NodeId
  | MoveBottomClick NodeId
  | NewGroupTop
  | StartRename NodeId String
  | EditInput String
  | EditKey String
  | CommitRename
  | CancelRename
  | DragStart NodeId
  | DragOver NodeId
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
      { api: Nothing, model: emptyModel, editing: Nothing, dragId: Nothing, dropTarget: Nothing, hover: Nothing, query: "", zoom: 1.0, notice: Nothing, scrollTop: 0.0, viewportH: 600.0, listener: Nothing, myWindow: Nothing, focusObserved: Nothing }
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
    -- learn which window this sidebar lives in, then reveal its active tab
    win <- H.liftAff (getCurrentWindowId api)
    H.modify_ _ { myWindow = win }
    revealFocus

  GotPatch p -> do
    H.modify_ \s -> s { model = applyPatch p s.model }
    revealFocus
  Scrolled m -> H.modify_ _ { scrollTop = m.top, viewportH = m.height }
  Remeasure -> do
    h <- H.liftEffect treeViewportHeight
    H.modify_ _ { viewportH = h }
  Toggle nid value -> sendCommand (Collapse nid value)
  ClickRow nid -> sendCommand (Activate nid)
  CloseClick nid -> sendCommand (CloseNode nid)
  DeleteClick nid -> sendCommand (Delete nid)
  FlattenClick nid -> sendCommand (Flatten nid)
  MoveTopLevelClick nid -> sendCommand (MoveTopLevel nid)
  MoveBottomClick nid -> sendCommand (MoveBottom nid)
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

  DragStart nid -> H.modify_ _ { dragId = Just nid, dropTarget = Nothing, hover = Nothing }
  -- Track the row under the cursor to drive the drop preview. dragover fires
  -- continuously, so only update (and re-render) when the target row changes.
  DragOver nid -> do
    st <- H.get
    when (st.dropTarget /= Just nid) (H.modify_ _ { dropTarget = Just nid })
  DragEnd -> H.modify_ _ { dragId = Nothing, dropTarget = Nothing }
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
    H.modify_ _ { dragId = Nothing, dropTarget = Nothing }

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

-- | Scroll the tree so this window's active tab is in view, following focus as it
-- | moves. We act only when the active-tab target *changes* (tracked in
-- | `focusObserved`), so unrelated patches don't re-scroll and the user's own
-- | scrolling is left alone. Skipped during search (the rows shown then are the
-- | search results, and auto-scrolling would fight typing). A target hidden in a
-- | collapsed group is left untracked, so expanding it later reveals it.
revealFocus :: forall o. H.HalogenM State Action () o Aff Unit
revealFocus = do
  st <- H.get
  case st.myWindow of
    Just w | st.query == "" -> case Scroll.activeTabInWindow w st.model of
      Nothing -> H.modify_ _ { focusObserved = Nothing }
      Just tid
        | st.focusObserved == Just tid -> pure unit
        | otherwise ->
            let entries = visible st.model
            in case Array.findIndex (\e -> e.id == tid) entries of
              Nothing -> pure unit
              Just idx -> do
                H.modify_ _ { focusObserved = Just tid }
                let
                  rowH = baseRowHeight * st.zoom
                  geom =
                    { rowHeight: rowH
                    , viewportHeight: st.viewportH
                    , contentHeight: Int.toNumber (Array.length entries) * rowH
                    , scrollTop: st.scrollTop
                    }
                case Scroll.revealScrollTop geom idx of
                  Just top -> H.liftEffect (scrollTreeTo top)
                  Nothing -> pure unit
    _ -> pure unit

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
                  (Array.mapWithIndex slot slice <> dropSlots)
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
  lastRoot = Array.last st.model.roots
  slot i entry = Tuple entry.id $ case Map.lookup entry.id st.model.nodes of
    Nothing -> HH.text ""
    Just node -> renderNode (st.dragId == Just node.id) st.editing guide (firstIdx + i) entry.depth rowH (Just node.id == lastRoot) node
  -- a single guide-styled insertion line marking where a drop would land
  dropSlots = case st.dragId, st.dropTarget of
    Just dragId, Just targetId -> case dropPlacement st.model entries dragId targetId of
      Just dp ->
        [ Tuple "drop-indicator"
            ( HH.div
                [ HP.class_ (ClassName "drop-indicator")
                , HP.style ("top:" <> show (Int.toNumber dp.atIndex * rowH) <> "px;--depth:" <> show dp.depth)
                ]
                []
            )
        ]
      Nothing -> []
    _, _ -> []
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

renderNode :: Boolean -> Maybe Editing -> Guide -> Int -> Int -> Number -> Boolean -> Node -> H.ComponentHTML Action () Aff
renderNode dragging editing guide idx depth rowH isLastRoot n =
  HH.div
    [ HP.classes (map ClassName (rowClasses dragging n))
    , HP.attr (AttrName "data-node-id") n.id
    , HP.attr (AttrName "data-status") (statusClass n)
    , HP.attr (AttrName "role") "treeitem"
    , HP.style
        ( "position:absolute;left:0;right:0;height:" <> show rowH
            <> "px;top:" <> show (Int.toNumber idx * rowH)
            <> "px;--depth:" <> show depth
        )
    , HP.draggable true
    , HE.onMouseEnter \_ -> SetHover (Just n.id)
    , HE.onDragStart \_ -> DragStart n.id
    , HE.onDragOver \_ -> DragOver n.id
    , HE.onDrop \_ -> DropOn n.id
    , HE.onDragEnd \_ -> DragEnd
    ]
    -- The guide layer is ALWAYS present (just empty when this row has no guide)
    -- and last. That keeps every row's child list structurally identical across
    -- hovers: hovering only mutates the guide layer's own children (the lines),
    -- never inserts/removes a sibling of the toggle/title/actions. Otherwise the
    -- DOM mutation races a pointer hit-test on the hover-revealed action buttons.
    -- It paints behind the content via z-index, not DOM order.
    [ toggleEl n, body editing n, actionsEl isLastRoot n, guideLayer guide idx ]

rowClasses :: Boolean -> Node -> Array String
rowClasses dragging n =
  [ "row", statusClass n, kindClass n ]
    <> (if n.active && isLive n then [ "active" ] else [])
    <> (if dragging then [ "dragging" ] else [])

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

-- Hover-revealed action cluster (rename / close / flatten / move-to-top-level /
-- move-to-bottom / delete). `isLastRoot` gates "move to bottom": there is nowhere
-- below the last top-level node to send it.
actionsEl :: Boolean -> Node -> H.ComponentHTML Action () Aff
actionsEl isLastRoot n = HH.span [ HP.class_ (ClassName "node-actions") ] (buttons isLastRoot n)

buttons :: Boolean -> Node -> Array (H.ComponentHTML Action () Aff)
buttons isLastRoot n =
  [ btn "btn-rename" "Rename" "pencil" (StartRename n.id (displayTitle n)) ]
    <> (if isLive n then [ btn "btn-close" "Close" "close-circle" (CloseClick n.id) ] else [])
    <> (if n.kind == KGroup then [ btn "btn-flatten" "Flatten" "flatten" (FlattenClick n.id) ] else [])
    -- "To top level" applies to any NESTED node (a top-level node has nowhere up to
    -- go); "to bottom" applies to any node that isn't already the last root. Both are
    -- offered on every kind, including live tabs (which get promoted into their own
    -- new window, like a drag to the root).
    <> (if isJust n.parent then [ btn "btn-to-top-level" "Move to top level" "to-top-level" (MoveTopLevelClick n.id) ] else [])
    <> (if not isLastRoot then [ btn "btn-to-bottom" "Move to bottom" "to-bottom" (MoveBottomClick n.id) ] else [])
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

-- | Live (has a browser binding) vs closed (restorable history) — drives the
-- | greyed-out styling. Derived, since liveness is no longer stored.
statusClass :: Node -> String
statusClass n = if isLive n then "live" else "closed"

-- | A container reads as a window exactly while it is live (a `KGroup`'s only
-- | binding is its `windowId`, so `isLive` ⟺ live window); otherwise a folder.
kindClass :: Node -> String
kindClass n = case n.kind of
  KTab -> "kind-tab"
  KGroup -> if isLive n then "kind-window" else "kind-group"

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

-- Subtree guide overlay (pure geometry lives in Model.Guide) ------------------

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
