-- | The sidebar: a thin Halogen *window* onto the background-owned forest. It
-- | holds no model — only the rows currently in view. It asks the background for
-- | a window of the visible order (`GetView`), renders exactly those rows
-- | (absolutely positioned, so the scrollbar reflects the full height), and
-- | re-fetches on scroll, resize, query, or a background `invalidate` ping. Every
-- | user action is a request; the resulting `invalidate` refreshes the window.
-- | Memory and per-open cost are O(window), independent of tree size.
module Sidebar.Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Int.Bits (and)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay)
import Effect.BootCache as BootCache
import Effect.Browser (BrowserApi, getBrowser, getCurrentWindowId)
import Effect.Channel (onBroadcast, request)
import Effect.Profile as Profile
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
import Model.Codec (Snapshot, decodeSnapshot)
import Model.Command (Command(..), Request(..), encodeRequest)
import Model.Drop (dropPlacement)
import Model.Guide (Guide, buildGuide, emptyGuide, guideBottom, guideTop)
import Model.PortableImport (portableToSnapshot)
import Model.Scroll as Scroll
import Model.Shortcuts as Sh
import Model.Types (Kind(..), NodeId)
import Model.View (ViewRow, decodeView)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (key)

foreign import allowDrops :: Effect Unit
foreign import keepFocused :: Effect Unit
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

-- height of one row in px at zoom 1 (matches the original's compact 18px).
baseRowHeight :: Number
baseRowHeight = 18.0

-- extra rows fetched/rendered above and below the viewport for smooth scrolling
overscan :: Int
overscan = 8

clampZoom :: Number -> Number
clampZoom = clamp 0.6 2.5

main :: Effect Unit
main = HA.runHalogenAff do
  bodyEl <- HA.awaitBody
  void $ runUI component unit bodyEl

type Editing = { id :: NodeId, text :: String }

type State =
  { api :: Maybe BrowserApi
  , total :: Int
  , rows :: Array ViewRow
  , reqStart :: Int -- start index of the currently-loaded window
  , editing :: Maybe Editing
  , dragId :: Maybe NodeId
  , dragSpan :: Maybe { index :: Int, subtreeEnd :: Int } -- dragged node's visible span, for the cycle-safe preview
  , dropTarget :: Maybe NodeId
  , hover :: Maybe NodeId
  , query :: String
  , zoom :: Number
  , notice :: Maybe String
  , scrollTop :: Number
  , viewportH :: Number
  , listener :: Maybe (HS.Listener Action)
  , myWindow :: Maybe Int
  , focusObserved :: Maybe Int -- last revealed focus index, so we follow focus without re-scrolling
  , profiling :: Boolean
  , bootProfiled :: Boolean -- the open profile is recorded once, on the first window load
  , opened :: Boolean -- false until the first live window loads; while false we land at the bottom
  }

data Action
  = Initialize
  | Invalidate
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
  | DragStart ViewRow
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
      { api: Nothing, total: 0, rows: [], reqStart: 0, editing: Nothing, dragId: Nothing, dragSpan: Nothing
      , dropTarget: Nothing, hover: Nothing, query: "", zoom: 1.0, notice: Nothing, scrollTop: 0.0
      , viewportH: 600.0, listener: Nothing, myWindow: Nothing, focusObserved: Nothing
      , profiling: false, bootProfiled: false, opened: false
      }
  , render
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- profiling: `nowMs` here is time since this document started loading, i.e. the
    -- bundle download + parse + eval + Halogen bootstrap before we ran.
    t0 <- H.liftEffect Profile.nowMs
    prof <- H.liftEffect Profile.getEnabled
    when prof (H.liftEffect Profile.clearBuffer)
    api <- H.liftEffect getBrowser
    H.liftEffect allowDrops
    H.liftEffect keepFocused
    z <- H.liftEffect getZoom
    { emitter, listener } <- H.liftEffect HS.create
    H.modify_ _ { api = Just api, zoom = clampZoom z, listener = Just listener }
    -- any background broadcast means "the model changed, re-fetch your window"
    H.liftEffect $ onBroadcast api \_ -> HS.notify listener Invalidate
    void $ H.subscribe emitter
    H.liftEffect $ onResize (HS.notify listener Remeasure)
    H.liftEffect $ Settings.onShortcut \combo -> case editorShortcut combo of
      Just act -> HS.notify listener act $> true
      Nothing -> do
        overrides <- Settings.getShortcuts
        case Sh.cmdForCombo overrides combo of
          Just cmd -> HS.notify listener (RunShortcut cmd) $> true
          Nothing -> pure false
    h <- H.liftEffect treeViewportHeight
    win <- H.liftAff (getCurrentWindowId api)
    tSetup <- H.liftEffect Profile.nowMs
    H.modify_ _ { viewportH = h, myWindow = win, profiling = prof }
    when prof $ H.liftEffect do
      Profile.record "boot.bootstrap" t0 -- doc load -> Initialize (bundle eval + Halogen)
      Profile.record "boot.setup" (tSetup - t0) -- subscribe / measure / window id
    -- Paint instantly from the last cached *bottom* window (the open default). A
    -- fresh open against a suspended background otherwise waits for it to wake +
    -- reload the whole model (~½s on a big tree); this shows content immediately,
    -- then requestView swaps in the live window (and reveals the active tab).
    cached <- H.liftEffect BootCache.load
    case jsonParser cached >>= decodeView of
      Right v -> do
        let sb = max 0.0 (Int.toNumber v.total * (baseRowHeight * clampZoom z) - h)
        H.modify_ _ { total = v.total, rows = v.rows, scrollTop = sb }
        H.liftEffect (scrollTreeTo sb)
        when prof (H.liftEffect (Profile.nowMs >>= Profile.record "boot.cached"))
      Left _ -> pure unit
    requestView true

  Invalidate -> requestView true
  Remeasure -> do
    h <- H.liftEffect treeViewportHeight
    H.modify_ _ { viewportH = h }
    requestView false
  Scrolled m -> do
    st <- H.get
    H.modify_ _ { scrollTop = m.top, viewportH = m.height }
    -- only re-fetch when scrolled ~overscan rows from the loaded window edge
    let
      rowH = baseRowHeight * st.zoom
      newStart = max 0 (Int.floor (m.top / rowH) - overscan)
      d = newStart - st.reqStart
    when (d >= overscan || d <= -overscan) (requestView false)

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

  DragStart r -> H.modify_ _ { dragId = Just r.id, dragSpan = Just { index: r.index, subtreeEnd: r.subtreeEnd }, dropTarget = Nothing, hover = Nothing }
  DragOver nid -> do
    st <- H.get
    when (st.dropTarget /= Just nid) (H.modify_ _ { dropTarget = Just nid })
  DragEnd -> H.modify_ _ { dragId = Nothing, dragSpan = Nothing, dropTarget = Nothing }
  SetHover mh -> H.modify_ \s -> case s.editing of
    Just _ -> s
    Nothing -> s { hover = mh }
  DropOn targetId -> do
    st <- H.get
    case st.dragId of
      Just dragId | dragId /= targetId -> sendCommand (Drop dragId targetId)
      _ -> pure unit
    H.modify_ _ { dragId = Nothing, dragSpan = Nothing, dropTarget = Nothing }

  SetQuery q -> do
    H.modify_ _ { query = q, scrollTop = 0.0, focusObserved = Nothing }
    H.liftEffect (scrollTreeTo 0.0)
    requestView false
  Zoom factor -> do
    st <- H.get
    let z = clampZoom (st.zoom * factor)
    H.modify_ _ { zoom = z }
    H.liftEffect (setZoom z)
    requestView false
  ExportClick -> do
    st <- H.get
    case st.api of
      Just api -> do
        resp <- H.liftAff (attempt (request api (encodeRequest Export)))
        case resp of
          Right json -> H.liftEffect (downloadJson "tabs-outliner.json" (stringify json))
          _ -> pure unit
      Nothing -> pure unit
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
      requestView false
    Sh.Export -> handleAction ExportClick
    Sh.Import -> handleAction ImportClick

-- | Fetch the window around the current scroll position and install it. When
-- | `reveal`, also scroll this window's active tab into view — but only when its
-- | index *changed* (tracked in `focusObserved`), so unrelated updates and the
-- | user's own scrolling are left alone, and never during search.
requestView :: forall o. Boolean -> H.HalogenM State Action () o Aff Unit
requestView reveal = attemptView reveal 20

-- | Fetch the window; on failure (a suspended background still waking up, a flaky
-- | wake-delivery) retry a bounded number of times. The retry is *forked* so it
-- | never blocks the action queue, and each attempt re-reads state so it always
-- | fetches the current window. The background's post-boot ping is the primary
-- | recovery path; this is the belt-and-suspenders.
attemptView :: forall o. Boolean -> Int -> H.HalogenM State Action () o Aff Unit
attemptView reveal n = do
  st <- H.get
  case st.api of
    Nothing -> pure unit
    Just api -> do
      let
        rowH = baseRowHeight * st.zoom
        -- +1 covers the partial rows at both viewport edges
        count = Int.ceil (st.viewportH / rowH) + overscan * 2 + 1
        -- land at the bottom on the first load (new windows / live nodes are there);
        -- after that the window simply follows the scroll position. Never while
        -- searching — results read top-down, so a query starts at the top.
        tail = reveal && not st.opened && st.query == ""
        start = max 0 (Int.floor (st.scrollTop / rowH) - overscan)
        vr = { start, count, query: st.query, myWindow: st.myWindow, wantFocus: reveal && st.query == "", tail }
      tReq <- if st.profiling then H.liftEffect Profile.nowMs else pure 0.0
      resp <- H.liftAff (attempt (request api (encodeRequest (GetView vr))))
      tFetch <- if st.profiling then H.liftEffect Profile.nowMs else pure 0.0
      case resp of
        Right json -> case decodeView json of
          Right v -> do
            tDecode <- if st.profiling then H.liftEffect Profile.nowMs else pure 0.0
            -- `tail` ignores `start`; the bg served the last window, so mirror its start.
            let actualStart = if tail then max 0 (v.total - count) else start
            H.modify_ _ { total = v.total, rows = v.rows, reqStart = actualStart, opened = true }
            when tail do
              let sb = max 0.0 (Int.toNumber v.total * rowH - st.viewportH)
              H.modify_ _ { scrollTop = sb }
              H.liftEffect (scrollTreeTo sb)
            -- cache the bottom window so the next (possibly cold) open paints it instantly
            when (actualStart + count >= v.total && st.query == "") (H.liftEffect (BootCache.save (stringify json)))
            when (reveal && st.query == "") (maybeReveal v.focusIndex)
            -- record the open profile once, when the FIRST window actually loads
            -- (which on a cold/suspended background is after it has woken + loaded,
            -- so boot.firstWindow / boot.paint reveal that wait).
            when (st.profiling && not st.bootProfiled) do
              H.modify_ _ { bootProfiled = true }
              H.liftEffect do
                Profile.record "boot.firstWindow" tFetch -- absolute: doc load -> window in hand
                Profile.record "boot.fetch" (tFetch - tReq) -- the (final) GetView round-trip
                Profile.record "boot.server" v.serverMs -- background compute within it
                Profile.record "boot.decode" (tDecode - tFetch) -- argonaut decode of the view
                Profile.finishBoot "sidebar.open" -- first paint mark + persist
          Left _ -> retry
        Left _ -> retry
  where
  retry = when (n > 1) $ void $ H.fork do
    H.liftAff (delay (Milliseconds 200.0))
    attemptView reveal (n - 1)

maybeReveal :: forall o. Int -> H.HalogenM State Action () o Aff Unit
maybeReveal fi = do
  st <- H.get
  when (fi >= 0 && Just fi /= st.focusObserved) do
    H.modify_ _ { focusObserved = Just fi }
    let
      rowH = baseRowHeight * st.zoom
      geom = { rowHeight: rowH, viewportHeight: st.viewportH, contentHeight: Int.toNumber st.total * rowH, scrollTop: st.scrollTop }
    case Scroll.revealScrollTop geom fi of
      Just top -> H.liftEffect (scrollTreeTo top)
      Nothing -> pure unit

sendCommand :: forall o. Command -> H.HalogenM State Action () o Aff Unit
sendCommand = sendRequest <<< RunCommand

-- | Fire a request and forget the reply: the window refreshes when the resulting
-- | `invalidate` broadcasts back.
sendRequest :: forall o. Request -> H.HalogenM State Action () o Aff Unit
sendRequest req = do
  st <- H.get
  case st.api of
    Just api -> void $ H.liftAff (attempt (request api (encodeRequest req)))
    Nothing -> pure unit

editorShortcut :: String -> Maybe Action
editorShortcut = case _ of
  "Ctrl+z" -> Just RunUndo
  "Meta+z" -> Just RunUndo
  "Ctrl+Shift+z" -> Just RunRedo
  "Shift+Meta+z" -> Just RunRedo
  "Ctrl+y" -> Just RunRedo
  "Meta+y" -> Just RunRedo
  _ -> Nothing

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
          [ HH.input [ HP.id "search", HP.placeholder "Search", HP.value st.query, HE.onValueInput SetQuery ]
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
                  (Array.mapWithIndex slot st.rows <> dropSlots)
              ]
          ]
    )
  where
  rowH = baseRowHeight * st.zoom
  totalH = Int.toNumber st.total * rowH
  -- guide over the window rows (window-relative indices); off-window parts clip
  -- harmlessly since they aren't rendered anyway.
  windowEntries = map (\r -> { id: r.id, depth: r.depth }) st.rows
  guide = case st.hover, st.editing of
    Just h, Nothing -> case Array.findIndex (\r -> r.id == h) st.rows of
      Just hi -> buildGuide windowEntries hi
      Nothing -> emptyGuide
    _, _ -> emptyGuide
  slot wi r = Tuple r.id (renderRow (st.dragId == Just r.id) st.editing guide wi rowH r)
  dropSlots = case st.dragId, st.dropTarget, st.dragSpan of
    Just dragId, Just targetId, Just span | dragId /= targetId ->
      case Array.find (\r -> r.id == targetId) st.rows >>= dropPlacement span of
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
    _, _, _ -> []
  iconBtn i label name act =
    HH.button [ HP.id i, HP.title label, HP.attr (AttrName "aria-label") label, HE.onClick \_ -> act ] [ toolbarIcon name ]
  textBtn i label glyph act =
    HH.button [ HP.id i, HP.title label, HE.onClick \_ -> act ] [ HH.text glyph ]

noticeBanner :: Maybe String -> Array (H.ComponentHTML Action () Aff)
noticeBanner = case _ of
  Nothing -> []
  Just msg -> [ HH.div [ HP.id "notice", HE.onClick \_ -> ClearNotice ] [ HH.text (msg <> "   ✕") ] ]

renderRow :: Boolean -> Maybe Editing -> Guide -> Int -> Number -> ViewRow -> H.ComponentHTML Action () Aff
renderRow dragging editing guide wi rowH r =
  HH.div
    [ HP.classes (map ClassName (rowClasses dragging r))
    , HP.attr (AttrName "data-node-id") r.id
    , HP.attr (AttrName "data-status") (statusClass r)
    , HP.attr (AttrName "role") "treeitem"
    , HP.style
        ( "position:absolute;left:0;right:0;height:" <> show rowH
            <> "px;top:" <> show (Int.toNumber r.index * rowH)
            <> "px;--depth:" <> show r.depth
        )
    , HP.draggable true
    , HE.onMouseEnter \_ -> SetHover (Just r.id)
    , HE.onDragStart \_ -> DragStart r
    , HE.onDragOver \_ -> DragOver r.id
    , HE.onDrop \_ -> DropOn r.id
    , HE.onDragEnd \_ -> DragEnd
    ]
    [ toggleEl r, body editing r, actionsEl r, guideLayer guide wi ]

rowClasses :: Boolean -> ViewRow -> Array String
rowClasses dragging r =
  [ "row", statusClass r, kindClass r ]
    <> (if r.active && r.live then [ "active" ] else [])
    <> (if dragging then [ "dragging" ] else [])

body :: Maybe Editing -> ViewRow -> H.ComponentHTML Action () Aff
body editing r = case editing of
  Just e | e.id == r.id ->
    HH.input
      [ HP.class_ (ClassName "rename-input")
      , HP.value e.text
      , HE.onValueInput EditInput
      , HE.onKeyDown (EditKey <<< key)
      , HE.onBlur \_ -> CommitRename
      ]
  _ ->
    HH.span [ HP.class_ (ClassName "title"), HE.onClick \_ -> ClickRow r.id ] [ HH.text r.title ]

actionsEl :: ViewRow -> H.ComponentHTML Action () Aff
actionsEl r = HH.span [ HP.class_ (ClassName "node-actions") ] (buttons r)

buttons :: ViewRow -> Array (H.ComponentHTML Action () Aff)
buttons r =
  [ btn "btn-rename" "Rename" "pencil" (StartRename r.id r.title) ]
    <> (if r.live then [ btn "btn-close" "Close" "close-circle" (CloseClick r.id) ] else [])
    <> (if r.kind == KGroup then [ btn "btn-flatten" "Flatten" "flatten" (FlattenClick r.id) ] else [])
    <> (if r.depth > 0 then [ btn "btn-to-top-level" "Move to top level" "root-outdent" (MoveTopLevelClick r.id) ] else [])
    <> (if not r.isLastRoot then [ btn "btn-to-bottom" "Move to bottom" "root-down" (MoveBottomClick r.id) ] else [])
    <> [ btn "btn-delete" "Delete" "trash" (DeleteClick r.id) ]
  where
  btn cls label name act =
    HH.button
      [ HP.class_ (ClassName cls), HP.title label, HP.attr (AttrName "aria-label") label, HE.onClick \_ -> act ]
      [ icon name ]

toggleEl :: ViewRow -> H.ComponentHTML Action () Aff
toggleEl r
  | not r.hasChildren = HH.span [ HP.class_ (ClassName "spacer") ] []
  | otherwise =
      HH.span [ HP.class_ (ClassName "toggle"), HE.onClick \_ -> Toggle r.id (not r.collapsed) ]
        [ icon (if r.collapsed then "chevron-right" else "chevron-down") ]

statusClass :: ViewRow -> String
statusClass r = if r.live then "live" else "closed"

kindClass :: ViewRow -> String
kindClass r = case r.kind of
  KTab -> "kind-tab"
  KGroup -> if r.live then "kind-window" else "kind-group"

-- Icons -----------------------------------------------------------------------

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

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
          [ HP.classes (map ClassName [ "guide-line", "guide-horizontal" ]), HP.style ("--guide-depth:" <> show depth) ]
          []
      ]
