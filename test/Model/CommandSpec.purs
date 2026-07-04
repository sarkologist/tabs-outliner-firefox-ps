module Test.Model.CommandSpec where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Model.Codec (Snapshot)
import Model.Command (BrowserAction(..), Command(..), applyCommand, wrapRootTabsModel)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Tree (applyPatch, insertAtClamped, liveTabCountInWindow, liveTabPreorder, liveWindowNode)
import Model.Types (Kind(..), Model, NodeId, defaultNode, emptyModel, isLive, isLiveTab)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

openTab :: Int -> Int -> Int -> String -> Boolean -> BrowserEvent
openTab tabId windowId index title active =
  TabOpened { tabId, windowId, openerTabId: Nothing, index, url: Just ("http://" <> title), title, active, favIconUrl: Nothing }

-- a TabOpened with an explicit url, to model the browser reporting a different url
-- for a recreated tab than the one stored
openTabU :: Int -> Int -> Int -> String -> String -> BrowserEvent
openTabU tabId windowId index url title =
  TabOpened { tabId, windowId, openerTabId: Nothing, index, url: Just url, title, active: false, favIconUrl: Nothing }

runEvents :: Array BrowserEvent -> Model
runEvents = foldl (\m e -> (applyBrowser 0.0 e m).model) emptyModel

-- window n1 with live tabs n2 (tab 11, "A") and n3 (tab 12, "B")
base :: Model
base = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false ]

-- base plus a second window n4 (id 2) holding a live tab n5 (tab 21, "C")
base2 :: Model
base2 = runEvents [ openTab 11 1 0 "A" true, openTab 12 1 1 "B" false, openTab 21 2 0 "C" true ]

run :: Command -> Model -> Model
run c m = (applyCommand 0.0 c m).model

-- Close a live tab the way the outliner's "Close (keep history)" does: emit the
-- removal, then feed the resulting browser onRemoved back as an outliner-initiated
-- close, so the node is KEPT as closed history. (A plain browser close of a fresh,
-- never-restored tab now drops it, so this is how a test makes a closed entry.)
outlinerClose :: NodeId -> Int -> Model -> Model
outlinerClose nid tabId m =
  let saved = (applyCommand 0.0 (CloseNode nid) m).model
  in (applyBrowser 0.0 (TabClosed { tabId }) saved).model

restoreIds :: Array NodeId
restoreIds = [ "a", "b", "c", "d" ]

tabTitle :: NodeId -> String
tabTitle = case _ of
  "a" -> "A"
  "b" -> "B"
  "c" -> "C"
  "d" -> "D"
  other -> other

savedGroupModel :: Model
savedGroupModel =
  (applyPatch
    { upserts:
        [ (defaultNode "g" KGroup 0.0) { title = "Saved", children = restoreIds }
        , savedTab "a"
        , savedTab "b"
        , savedTab "c"
        , savedTab "d"
        ]
    , removes: []
    , roots: Just [ "g" ]
    }
    emptyModel
  ) { nextId = 10 }
  where
  savedTab id =
    let title = tabTitle id
    in (defaultNode id KTab 0.0) { title = title, url = Just ("http://" <> title), parent = Just "g", closedAt = Just 0.0 }

pmod :: Int -> Int -> Int
pmod a b = if b <= 0 then 0 else ((a `mod` b) + b) `mod` b

restoreOrder :: Array Int -> Array NodeId
restoreOrder raw = go raw restoreIds []
  where
  go _ [] acc = acc
  go choices remaining acc =
    let
      choice = case Array.uncons choices of
        Just { head } -> head
        Nothing -> 0
      restChoices = case Array.uncons choices of
        Just { tail } -> tail
        Nothing -> []
      id = fromMaybe "a" (Array.index remaining (pmod choice (Array.length remaining)))
    in
      go restChoices (Array.delete id remaining) (Array.snoc acc id)

feedEvents :: Model -> Array BrowserEvent -> Model
feedEvents = foldl (\m e -> (applyBrowser 0.0 e m).model)

liveChildIds :: Model -> NodeId -> Array NodeId
liveChildIds m parent = liveTabPreorder m parent

type RestoreSim =
  { model :: Model
  , browser :: Array NodeId
  , nextTab :: Int
  , windowId :: Maybe Int
  }

restoreOne :: RestoreSim -> NodeId -> RestoreSim
restoreOne s nid =
  let r = applyCommand 0.0 (Activate nid) s.model
  in case Array.uncons r.actions of
    Just { head: CreateWindow _, tail } | Array.null tail ->
      let
        wid = fromMaybe 50 s.windowId
        tabId = s.nextTab
        title = tabTitle nid
        model' = feedEvents r.model
          [ WindowOpened { windowId: wid }
          , TabOpened { tabId, windowId: wid, openerTabId: Nothing, index: 0, url: Just ("http://" <> title), title, active: true, favIconUrl: Nothing }
          ]
      in
        { model: model', browser: [ nid ], nextTab: tabId + 1, windowId: Just wid }
    Just { head: CreateTab (Just wid) index _, tail } | Array.null tail ->
      let
        tabId = s.nextTab
        title = tabTitle nid
        i = fromMaybe (Array.length s.browser) index
        model' = feedEvents r.model
          [ TabOpened { tabId, windowId: wid, openerTabId: Nothing, index: i, url: Just ("http://" <> title), title, active: false, favIconUrl: Nothing } ]
      in
        { model: model', browser: insertAtClamped i nid s.browser, nextTab: tabId + 1, windowId: Just wid }
    _ -> s { model = r.model }

type SimTab =
  { tabId :: Int
  , windowId :: Int
  , url :: Maybe String
  , title :: String
  , active :: Boolean
  }

type SimWindow = { windowId :: Int, tabs :: Array SimTab }

type OrderCheck = { window :: Int, browser :: Array Int, model :: Array Int, history :: Array String }

type ActionCheck = { label :: String, expected :: Array BrowserAction, actual :: Array BrowserAction, history :: Array String }

type UserSim =
  { model :: Model
  , windows :: Array SimWindow
  , events :: Array BrowserEvent
  , activeWindow :: Maybe Int
  , nextTab :: Int
  , nextWindow :: Int
  , failures :: Array OrderCheck
  , actionFailures :: Array ActionCheck
  , history :: Array String
  }

simTab :: Int -> Int -> String -> Boolean -> SimTab
simTab tabId windowId title active =
  { tabId, windowId, url: Just ("http://" <> title), title, active }

userSimInit :: UserSim
userSimInit =
  { model: base2
  , windows:
      [ { windowId: 1, tabs: [ simTab 11 1 "A" true, simTab 12 1 "B" false ] }
      , { windowId: 2, tabs: [ simTab 21 2 "C" true ] }
      ]
  , events: []
  , activeWindow: Just 1
  , nextTab: 100
  , nextWindow: 50
  , failures: []
  , actionFailures: []
  , history: []
  }

rawAt :: Array Int -> Int -> Int
rawAt raw i = fromMaybe 0 (Array.index raw i)

clampIndex :: Int -> Int -> Int
clampIndex i len
  | i < 0 = len
  | i > len = len
  | otherwise = i

nodeIds :: Model -> Array NodeId
nodeIds m = map _.id (Array.fromFoldable (Map.values m.nodes))

groupIds :: Model -> Array NodeId
groupIds m = Array.mapMaybe
  (\n -> if n.kind == KGroup then Just n.id else Nothing)
  (Array.fromFoldable (Map.values m.nodes))

pickMaybe :: forall a. Array a -> Int -> Maybe a
pickMaybe xs raw = Array.index xs (pmod raw (Array.length xs))

childCount :: Maybe NodeId -> Model -> Int
childCount parent m = case parent of
  Nothing -> Array.length m.roots
  Just pid -> fromMaybe 0 (Array.length <<< _.children <$> Map.lookup pid m.nodes)

userIndex :: Maybe NodeId -> Model -> Int -> Int
userIndex parent m raw = pmod raw (childCount parent m + 1)

findWindowIn :: Int -> Array SimWindow -> Maybe SimWindow
findWindowIn windowId = Array.find (\w -> w.windowId == windowId)

replaceWindowIn :: SimWindow -> Array SimWindow -> Array SimWindow
replaceWindowIn win wins =
  if Array.any (\w -> w.windowId == win.windowId) wins then
    map (\w -> if w.windowId == win.windowId then win else w) wins
  else Array.snoc wins win

findTabIn :: Int -> Array SimWindow -> Maybe { tab :: SimTab, window :: SimWindow }
findTabIn tabId wins = Array.head (Array.mapMaybe inWindow wins)
  where
  inWindow w = map (\t -> { tab: t, window: w }) (Array.find (\t -> t.tabId == tabId) w.tabs)

currentWindowId :: UserSim -> Maybe Int
currentWindowId s = case s.activeWindow of
  Just wid -> Just wid
  Nothing -> map _.windowId (Array.head s.windows)

ensureWindow :: Int -> UserSim -> { state :: UserSim, opened :: Boolean }
ensureWindow windowId s = case findWindowIn windowId s.windows of
  Just _ -> { state: s, opened: false }
  Nothing ->
    { state: s
        { windows = Array.snoc s.windows { windowId, tabs: [] }
        , nextWindow = max s.nextWindow (windowId + 1)
        }
    , opened: true
    }

insertTabInto :: Int -> SimTab -> SimWindow -> { window :: SimWindow, index :: Int }
insertTabInto requested tab win =
  let index = clampIndex requested (Array.length win.tabs)
  in { window: win { tabs = insertAtClamped index tab win.tabs }, index }

liveSlotAfterDetachIn :: Model -> NodeId -> NodeId -> Int -> Int
liveSlotAfterDetachIn m movingId parentId index = case Map.lookup movingId m.nodes, Map.lookup parentId m.nodes of
  Just moving, Just parent ->
    let
      children = if moving.parent == Just parent.id then Array.delete moving.id parent.children else parent.children
      slot = clamp 0 (Array.length children) index
    in
      foldl (\n cid -> n + liveTabCountInWindow m parent.id cid) 0 (Array.take slot children)
  _, _ -> index

expectedMoveActions :: String -> NodeId -> Maybe NodeId -> Int -> Model -> Maybe { label :: String, actions :: Array BrowserAction }
expectedMoveActions label nid mParent index m = case Map.lookup nid m.nodes, mParent of
  Just node, Just pid | isLiveTab node -> case node.tabId, Map.lookup pid m.nodes of
    Just tabId, Just parent | Just windowId <- parent.windowId ->
      Just { label, actions: [ MoveTabToWindow tabId windowId (liveSlotAfterDetachIn m nid pid index) ] }
    _, _ -> Nothing
  _, _ -> Nothing

expectedCommandActions :: Command -> Model -> Maybe { label :: String, actions :: Array BrowserAction }
expectedCommandActions cmd m = case cmd of
  Move nid parent index -> expectedMoveActions ("move " <> nid) nid parent index m
  Drop dragId targetId
    | dragId == targetId -> Nothing
    | otherwise -> case Map.lookup targetId m.nodes of
        Just target | target.kind == KGroup ->
          expectedMoveActions ("drop " <> dragId <> " onto group " <> targetId) dragId (Just target.id) (Array.length target.children) m
        Just target ->
          let
            siblings = case target.parent of
              Just pid -> fromMaybe [] (_.children <$> Map.lookup pid m.nodes)
              Nothing -> m.roots
            shrunk = Array.delete dragId siblings
            idx = fromMaybe (Array.length shrunk) (Array.elemIndex targetId shrunk)
          in
            expectedMoveActions ("drop " <> dragId <> " before " <> targetId) dragId target.parent idx m
        Nothing -> Nothing
  _ -> Nothing

enqueueEvents :: Array BrowserEvent -> UserSim -> UserSim
enqueueEvents evs s = s { events = s.events <> evs }

moveBrowserTab :: Int -> Int -> Int -> UserSim -> UserSim
moveBrowserTab tabId destWindow requested s = case findTabIn tabId s.windows of
  Nothing -> s
  Just found -> case findWindowIn destWindow s.windows of
    Nothing -> s
    Just _ ->
      let
        old = found.window
        sameWindow = old.windowId == destWindow
        oldWithout = old { tabs = Array.filter (\t -> t.tabId /= tabId) old.tabs }
        removed = s { windows = replaceWindowIn oldWithout s.windows }
        dest0 = fromMaybe { windowId: destWindow, tabs: [] } (findWindowIn destWindow removed.windows)
        requested' = if requested < 0 then Array.length dest0.tabs else requested
        inserted = insertTabInto requested' (found.tab { windowId = destWindow }) dest0
        withDest = removed
          { windows = replaceWindowIn inserted.window removed.windows
          , activeWindow = Just destWindow
          }
        oldEmptied = (not sameWindow) && Array.null oldWithout.tabs
        finalWindows =
          if oldEmptied then Array.filter (\w -> w.windowId /= old.windowId) withDest.windows
          else withDest.windows
        moveEvent =
          if sameWindow then TabMoved { tabId, windowId: destWindow, toIndex: inserted.index }
          else TabAttached { tabId, windowId: destWindow, index: inserted.index }
        closeEvents = if oldEmptied then [ WindowClosed { windowId: old.windowId } ] else []
      in
        enqueueEvents ([ moveEvent ] <> closeEvents) (withDest { windows = finalWindows })

removeBrowserTab :: Int -> UserSim -> UserSim
removeBrowserTab tabId s = case findTabIn tabId s.windows of
  Nothing -> s
  Just found ->
    let
      old = found.window
      oldWithout = old { tabs = Array.filter (\t -> t.tabId /= tabId) old.tabs }
      s' = s { windows = replaceWindowIn oldWithout s.windows }
    in
      enqueueEvents [ TabClosed { tabId } ] s'

applyBrowserAction :: Int -> UserSim -> BrowserAction -> UserSim
applyBrowserAction salt s = case _ of
  FocusTab tabId -> case findTabIn tabId s.windows of
    Nothing -> s
    Just found ->
      let
        window' = found.window { tabs = map (\t -> t { active = t.tabId == tabId }) found.window.tabs }
        s' = s { windows = replaceWindowIn window' s.windows, activeWindow = Just found.window.windowId }
      in
        enqueueEvents [ TabActivated { tabId, windowId: found.window.windowId } ] s'
  CreateTab mWindow mIndex mUrl ->
    case mWindow >>= \windowId -> if Array.any (\w -> w.windowId == windowId) s.windows then Nothing else Just windowId of
      Just _ -> s
      Nothing ->
        let
          windowId = fromMaybe (fromMaybe s.nextWindow (currentWindowId s)) mWindow
          ensured = ensureWindow windowId s
          win0 = fromMaybe { windowId, tabs: [] } (findWindowIn windowId ensured.state.windows)
          tabId = ensured.state.nextTab
          url = fromMaybe ("http://new" <> show tabId) mUrl
          requested = fromMaybe (Array.length win0.tabs) mIndex
          tab = { tabId, windowId, url: Just url, title: url, active: true }
          inserted = insertTabInto requested tab win0
          s' = ensured.state
            { windows = replaceWindowIn inserted.window ensured.state.windows
            , nextTab = tabId + 1
            , activeWindow = Just windowId
            }
          openEvents = if ensured.opened then [ WindowOpened { windowId } ] else []
        in
          enqueueEvents
            ( openEvents <>
                [ TabOpened { tabId, windowId, openerTabId: Nothing, index: inserted.index, url: Just url, title: url, active: true, favIconUrl: Nothing } ]
            )
            s'
  CreateWindow urls ->
    let
      windowId = s.nextWindow
      tabs = Array.mapWithIndex
        (\i url -> { tabId: s.nextTab + i, windowId, url: Just url, title: url, active: i == 0 })
        urls
      tabEvents = Array.mapWithIndex
        (\i url ->
          TabOpened
            { tabId: s.nextTab + i
            , windowId
            , openerTabId: Nothing
            , index: i
            , url: Just url
            , title: url
            , active: i == 0
            , favIconUrl: Nothing
            }
        )
        urls
      s' = s
        { windows = Array.snoc s.windows { windowId, tabs }
        , nextWindow = windowId + 1
        , nextTab = s.nextTab + Array.length urls
        , activeWindow = Just windowId
        }
      events =
        if pmod salt 2 == 0 then [ WindowOpened { windowId } ] <> tabEvents
        else tabEvents <> [ WindowOpened { windowId } ]
    in
      enqueueEvents events s'
  MoveTabToWindow tabId windowId index -> moveBrowserTab tabId windowId index s
  NewWindowWithTabs tabIds -> case Array.uncons tabIds of
    Nothing -> s
    Just _ ->
      let
        windowId = s.nextWindow
        s' = s
          { windows = Array.snoc s.windows { windowId, tabs: [] }
          , nextWindow = windowId + 1
          , activeWindow = Just windowId
          , events = s.events <> [ WindowOpened { windowId } ]
          }
      in
        foldl (\acc tabId -> moveBrowserTab tabId windowId (-1) acc) s' tabIds
  RemoveTab tabId -> removeBrowserTab tabId s

applySimCommand :: Int -> Command -> UserSim -> UserSim
applySimCommand salt cmd s =
  let
    expected = expectedCommandActions cmd s.model
    r = applyCommand 0.0 cmd s.model
    checked = case expected of
      Just e | e.actions /= r.actions ->
        s { actionFailures = Array.snoc s.actionFailures { label: e.label, expected: e.actions, actual: r.actions, history: s.history } }
      _ -> s
  in
    foldl (applyBrowserAction salt) (checked { model = r.model }) r.actions

flushOne :: UserSim -> UserSim
flushOne s = case Array.uncons s.events of
  Nothing -> s
  Just { head, tail } -> s { model = (applyBrowser 0.0 head s.model).model, events = tail }

flushAll :: UserSim -> UserSim
flushAll s =
  if Array.null s.events then settleCheck s
  else flushAll (flushOne s)

importSnapshot :: Int -> Snapshot
importSnapshot raw =
  let
    suffix = show (pmod raw 1000)
    gid = "ig" <> suffix
    aid = "ia" <> suffix
    bid = "ib" <> suffix
  in
    { nodes:
        [ (defaultNode gid KGroup 0.0) { title = "Imported" <> suffix, children = [ aid, bid ] }
        , (defaultNode aid KTab 0.0) { parent = Just gid, title = "IA" <> suffix, url = Just ("http://ia" <> suffix) }
        , (defaultNode bid KTab 0.0) { parent = Just gid, title = "IB" <> suffix, url = Just ("http://ib" <> suffix) }
        ]
    , roots: [ gid ]
    }

browserTabOrder :: Int -> UserSim -> Array Int
browserTabOrder windowId s = case findWindowIn windowId s.windows of
  Nothing -> []
  Just w -> map _.tabId w.tabs

modelTabOrder :: Int -> Model -> Array Int
modelTabOrder windowId m = case liveWindowNode windowId m of
  Nothing -> []
  Just w -> Array.mapMaybe tabIdIfLive (liveTabPreorder m w.id)
  where
  tabIdIfLive cid = Map.lookup cid m.nodes >>= \n -> if isLiveTab n then n.tabId else Nothing

orderChecks :: UserSim -> Array OrderCheck
orderChecks s =
  let
    browserWindows = map _.windowId s.windows
    modelWindows = Array.fromFoldable (Map.keys s.model.byWindow)
  in
    map
      (\window -> { window, browser: browserTabOrder window s, model: modelTabOrder window s.model, history: s.history })
      (Array.nub (browserWindows <> modelWindows))

orderMismatches :: UserSim -> Array OrderCheck
orderMismatches s = Array.filter (\c -> c.browser /= c.model) (orderChecks s)

settleCheck :: UserSim -> UserSim
settleCheck s =
  if Array.null s.events then s { failures = s.failures <> orderMismatches s }
  else s

simUserStep :: UserSim -> Array Int -> UserSim
simUserStep s raw =
  let
    op = pmod (rawAt raw 0) 14
    -- Non-activate commands are generated from settled UI/model states. Activate
    -- may run while create/restore events are still queued, which is the race that
    -- originally let restored tabs compute stale insertion indexes.
    effectiveOp = if not (Array.null s.events) && op /= 0 && op /= 1 && op /= 13 then 0 else op
    stepped = case effectiveOp of
      0 -> flushOne s
      1 -> onNode Activate
      2 -> onNode CloseNode
      3 -> onNode Delete
      4 ->
        let parents = [ Nothing ] <> map Just (groupIds s.model)
        in case pickMaybe (nodeIds s.model) (rawAt raw 1) of
          Nothing -> s
          Just nid ->
            let parent = fromMaybe Nothing (pickMaybe parents (rawAt raw 2))
            in applySimCommand salt (Move nid parent (userIndex parent s.model (rawAt raw 3))) s
      5 -> case pickMaybe (nodeIds s.model) (rawAt raw 1), pickMaybe (nodeIds s.model) (rawAt raw 2) of
        Just dragId, Just targetId -> applySimCommand salt (Drop dragId targetId) s
        _, _ -> s
      6 -> onNode MoveTopLevel
      7 -> onNode MoveBottom
      8 -> onNode Flatten
      9 ->
        let parents = [ Nothing ] <> map Just (groupIds s.model)
            parent = fromMaybe Nothing (pickMaybe parents (rawAt raw 1))
        in applySimCommand salt (NewGroup parent (userIndex parent s.model (rawAt raw 2))) s
      10 -> onNode (\nid -> Rename nid ("R" <> show (pmod (rawAt raw 2) 1000)))
      11 -> onNode (\nid -> Collapse nid (pmod (rawAt raw 2) 2 == 0))
      12 -> applySimCommand salt (Import (importSnapshot (rawAt raw 1))) s
      _ -> flushAll s
  in
    settleCheck (stepped { history = Array.snoc s.history (show effectiveOp <> ":" <> show (Array.take 4 raw)) })
  where
  salt = rawAt raw 5
  onNode f = case pickMaybe (nodeIds s.model) (rawAt raw 1) of
    Nothing -> s
    Just nid -> applySimCommand salt (f nid) s

spec :: Spec Unit
spec = describe "Model.Command" do
  it "collapse sets the flag" do
    (_.collapsed <$> Map.lookup "n1" (run (Collapse "n1" true) base).nodes) `shouldEqual` Just true

  it "expandAncestors opens every collapsed ancestor and ignores missing nodes" do
    let
      nested = applyPatch
        { upserts:
            [ (defaultNode "A" KGroup 0.0) { children = [ "B" ], collapsed = true }
            , (defaultNode "B" KGroup 0.0) { parent = Just "A", children = [ "C" ], collapsed = true }
            , (defaultNode "C" KTab 0.0) { parent = Just "B" }
            ]
        , removes: []
        , roots: Just [ "A" ]
        }
        emptyModel
      expanded = run (ExpandAncestors "C") nested
    (_.collapsed <$> Map.lookup "A" expanded.nodes) `shouldEqual` Just false
    (_.collapsed <$> Map.lookup "B" expanded.nodes) `shouldEqual` Just false
    run (ExpandAncestors "missing") nested `shouldEqual` nested

  it "rename sets a custom title" do
    (_.customTitle <$> Map.lookup "n2" (run (Rename "n2" "X") base).nodes) `shouldEqual` Just (Just "X")

  it "activate a live tab focuses it" do
    (applyCommand 0.0 (Activate "n2") base).actions `shouldEqual` [ FocusTab 11 ]

  it "close a window closes all its live tabs" do
    (applyCommand 0.0 (CloseNode "n1") base).actions `shouldEqual` [ RemoveTab 11, RemoveTab 12 ]

  it "delete removes the subtree and closes its live tabs" do
    let r = applyCommand 0.0 (Delete "n2") base
    Map.lookup "n2" r.model.nodes `shouldEqual` Nothing
    (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "n3" ]
    r.actions `shouldEqual` [ RemoveTab 11 ]

  it "move re-parents a (non-live) node to the root" do
    let
      m0 = run (NewGroup (Just "n1") 0) base -- group n4 as n1's first child
      m = run (Move "n4" Nothing 0) m0
    (_.parent <$> Map.lookup "n4" m.nodes) `shouldEqual` Just Nothing
    (_.children <$> Map.lookup "n1" m.nodes) `shouldEqual` Just [ "n2", "n3" ]
    m.roots `shouldEqual` [ "n4", "n1" ]

  it "new group then flatten promotes children and removes the group" do
    let
      m1 = run (NewGroup Nothing 0) base -- group n4 at roots[0]
      m2 = run (Move "n1" (Just "n4") 0) m1 -- window n1 under the group (a non-live move)
      m3 = run (Flatten "n4") m2
    (_.kind <$> Map.lookup "n4" m1.nodes) `shouldEqual` Just KGroup
    Map.lookup "n4" m3.nodes `shouldEqual` Nothing
    m3.roots `shouldEqual` [ "n1" ]
    (_.parent <$> Map.lookup "n1" m3.nodes) `shouldEqual` Just Nothing

  it "move into one's own descendant is rejected (no cycle)" do
    let m = run (Move "n1" (Just "n2") 0) base -- n2 is a child of n1
    (_.parent <$> Map.lookup "n1" m.nodes) `shouldEqual` Just Nothing

  it "flatten does nothing on a non-group" do
    let m = run (Flatten "n2") base -- n2 is a tab
    (_.kind <$> Map.lookup "n2" m.nodes) `shouldEqual` Just KTab
    Map.size m.nodes `shouldEqual` Map.size base.nodes

  it "flatten of a live window detaches its tabs into a new window and dissolves it" do
    let r = applyCommand 0.0 (Flatten "n1") base -- n1 is the live window (windowId 1) at root
    Map.lookup "n1" r.model.nodes `shouldEqual` Nothing -- the window node is gone...
    r.actions `shouldEqual` [ NewWindowWithTabs [ 11, 12 ] ] -- ...its tabs re-homed into one fresh window
    -- the promoted LIVE tabs sit at the root transiently (the browser action moves the
    -- real tabs; events re-home them) — they must NOT be wrapped in stray groups
    r.model.roots `shouldEqual` [ "n2", "n3" ]
    (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just Nothing

  it "flatten of a nested live window merges its tabs into the parent window" do
    let
      -- outer window P (id 2) holding its own tab pp and a nested window W (id 1)
      m = applyPatch
        { upserts:
            [ (defaultNode "P" KGroup 0.0) { windowId = Just 2, title = "Outer", children = [ "pp", "W" ] }
            , (defaultNode "pp" KTab 0.0) { parent = Just "P", tabId = Just 20, url = Just "http://p", title = "P0" }
            , (defaultNode "W" KGroup 0.0) { windowId = Just 1, parent = Just "P", title = "Inner", children = [ "t1", "t2" ] }
            , (defaultNode "t1" KTab 0.0) { parent = Just "W", tabId = Just 11, url = Just "http://a", title = "A" }
            , (defaultNode "t2" KTab 0.0) { parent = Just "W", tabId = Just 12, url = Just "http://b", title = "B" }
            ]
        , removes: []
        , roots: Just [ "P" ]
        }
        emptyModel
      r = applyCommand 0.0 (Flatten "W") m
    Map.lookup "W" r.model.nodes `shouldEqual` Nothing -- inner window dissolved
    r.actions `shouldEqual` [ MoveTabToWindow 11 2 1, MoveTabToWindow 12 2 2 ] -- merged at W's old slot in the outer window

  it "closing a window drops its window binding but leaves nested groups untouched" do
    let
      withGroup = run (NewGroup (Just "n1") 0) base -- group n4 under window n1
      closed = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) withGroup).model
    -- the window container is no longer live: its windowId binding is gone, so it
    -- now reads as a plain saved group
    (isLive <$> Map.lookup "n1" closed.nodes) `shouldEqual` Just false
    (_.windowId <$> Map.lookup "n1" closed.nodes) `shouldEqual` Just Nothing
    -- the nested user group never had a browser binding, so close leaves it untouched
    (_.kind <$> Map.lookup "n4" closed.nodes) `shouldEqual` Just KGroup
    (_.closedAt <$> Map.lookup "n4" closed.nodes) `shouldEqual` Just Nothing

  it "import adds an exported outline as inert, restorable top-level history" do
    let
      grp = (defaultNode "g1" KGroup 0.0) { title = "G", children = [ "t1" ] }
      tab = (defaultNode "t1" KTab 0.0) { title = "T", url = Just "http://t", tabId = Just 5, parent = Just "g1" }
      r = applyCommand 0.0 (Import { nodes: [ grp, tab ], roots: [ "g1" ] }) base
    -- fresh ids (base.nextId is 4): g1 -> n4, t1 -> n5; appended to roots
    r.model.roots `shouldEqual` [ "n1", "n4" ]
    (_.kind <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just KGroup
    -- every imported node is inert (no browser binding): the container is a plain
    -- saved group, the tab restorable history (keeps its url, drops its tabId)
    (isLive <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just false
    (_.children <$> Map.lookup "n4" r.model.nodes) `shouldEqual` Just [ "n5" ]
    (isLive <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just false
    (_.tabId <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just Nothing
    (_.url <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just (Just "http://t")
    (_.parent <$> Map.lookup "n5" r.model.nodes) `shouldEqual` Just (Just "n4")

  it "restore re-binds the existing node when the tab re-opens (no duplicate)" do
    let
      closed = outlinerClose "n2" 11 (runEvents [ openTab 11 1 0 "A" true ])
      activated = applyCommand 0.0 (Activate "n2") closed
      reopened = (applyBrowser 0.0 (openTab 99 1 0 "A" true) activated.model).model
    -- the window is still live, so the tab reopens back into it (not a new window)
    activated.actions `shouldEqual` [ CreateTab (Just 1) (Just 0) (Just "http://A") ]
    -- same node id, now live and bound to the new tab; no extra node created
    (isLive <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just true
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 99)
    Map.size reopened.nodes `shouldEqual` 2

  it "restoring into a live window counts earlier pending siblings" do
    let
      closedBoth = outlinerClose "n3" 12 (outlinerClose "n2" 11 base)
      withPending = closedBoth { pendingRestore = Map.insert 1 (Cons "n2" Nil) closedBoth.pendingRestore }
      activated = applyCommand 0.0 (Activate "n3") withPending
    activated.actions `shouldEqual` [ CreateTab (Just 1) (Just 1) (Just "http://B") ]
    Map.lookup 1 activated.model.pendingRestore `shouldEqual` Just (Cons "n2" (Cons "n3" Nil))

  it "restoring rebinds the clicked node even when the recreated tab reports a different url" do
    let
      closed = outlinerClose "n2" 11 (runEvents [ openTab 11 1 0 "A" true ])
      activated = applyCommand 0.0 (Activate "n2") closed
      -- the browser recreates the tab, but onCreated reports a normalized/redirected
      -- url ("http://A/" with a trailing slash, not the stored "http://A")
      reopened = (applyBrowser 0.0
        (TabOpened { tabId: 99, windowId: 1, openerTabId: Nothing, index: 0, url: Just "http://A/", title: "A", active: true, favIconUrl: Nothing })
        activated.model).model
    -- the SAME node n2 is rebound — no duplicate fresh node
    (isLive <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just true
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 99)
    Map.size reopened.nodes `shouldEqual` 2

  it "restoring a closed window opens a new window (not the active one)" do
    let
      closedWin = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) base).model
      activated = applyCommand 0.0 (Activate "n1") closedWin
    -- one new window carrying both tabs' urls, in order — no bare CreateTab
    activated.actions `shouldEqual` [ CreateWindow [ "http://A", "http://B" ] ]
    -- the closed window node is queued to rebind to the window that opens
    (map _.node activated.model.pendingRestoreWindows) `shouldEqual` [ "n1" ]

  it "the restored window node goes live in place when its window opens (no duplicate)" do
    let
      closedWin = (applyBrowser 0.0 (WindowClosed { windowId: 1 }) base).model
      activated = applyCommand 0.0 (Activate "n1") closedWin
      -- the browser opens the new window (id 2) and re-creates both tabs in it
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 2 }
        , openTab 21 2 0 "A" true
        , openTab 22 2 1 "B" false
        ]
    -- the existing window node n1 is now live and bound to the new browser window
    (isLive <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just true
    (_.windowId <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just (Just 2)
    reopened.pendingRestoreWindows `shouldEqual` []
    -- its tabs re-bound to their existing nodes, still under n1, all live
    (isLive <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just true
    (isLive <$> Map.lookup "n3" reopened.nodes) `shouldEqual` Just true
    (_.children <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just [ "n2", "n3" ]
    -- no phantom window node, no duplicated tabs
    reopened.roots `shouldEqual` [ "n1" ]
    Map.size reopened.nodes `shouldEqual` 3

  it "restoring a closed window with nested tabs opens one window in preorder" do
    let
      -- closed window n1 = [ A(n2 -> B(n3)), C(n4) ]; tab nesting is semantic, not
      -- a browser-window boundary, so all three tabs restore into n1's one window.
      m0 = applyPatch
        { upserts:
            [ (defaultNode "n1" KGroup 0.0) { title = "W", children = [ "n2", "n4" ] }
            , (defaultNode "n2" KTab 0.0) { parent = Just "n1", children = [ "n3" ], url = Just "http://a", title = "A" }
            , (defaultNode "n3" KTab 0.0) { parent = Just "n2", url = Just "http://b", title = "B" }
            , (defaultNode "n4" KTab 0.0) { parent = Just "n1", url = Just "http://c", title = "C" }
            ]
        , removes: []
        , roots: Just [ "n1" ]
        }
        emptyModel
      activated = applyCommand 0.0 (Activate "n1") m0
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 5 }
        , openTab 51 5 0 "a" true
        , openTab 52 5 1 "b" false
        , openTab 53 5 2 "c" false
        ]
    activated.actions `shouldEqual` [ CreateWindow [ "http://a", "http://b", "http://c" ] ]
    (map _.node activated.model.pendingRestoreWindows) `shouldEqual` [ "n1" ]
    (map _.tabs activated.model.pendingRestoreWindows) `shouldEqual` [ Cons "n2" (Cons "n3" (Cons "n4" Nil)) ]
    (_.windowId <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just (Just 5)
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 51)
    (_.tabId <$> Map.lookup "n3" reopened.nodes) `shouldEqual` Just (Just 52)
    (_.tabId <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just (Just 53)
    liveChildIds reopened "n1" `shouldEqual` [ "n2", "n3", "n4" ]

  it "restoring a nested tab into a live window uses preorder for the browser index" do
    let
      m0 = applyPatch
        { upserts:
            [ (defaultNode "n1" KGroup 0.0) { windowId = Just 1, title = "W", children = [ "n2", "n4" ] }
            , (defaultNode "n2" KTab 0.0) { parent = Just "n1", children = [ "n3" ], tabId = Just 11, url = Just "http://a", title = "A" }
            , (defaultNode "n3" KTab 0.0) { parent = Just "n2", url = Just "http://b", title = "B" }
            , (defaultNode "n4" KTab 0.0) { parent = Just "n1", tabId = Just 12, url = Just "http://c", title = "C" }
            ]
        , removes: []
        , roots: Just [ "n1" ]
        }
        emptyModel
      activated = applyCommand 0.0 (Activate "n3") m0
    activated.actions `shouldEqual` [ CreateTab (Just 1) (Just 1) (Just "http://b") ]

  it "restoring into an outer live window ignores nested live-window tabs in the browser index" do
    let
      m0 = applyPatch
        { upserts:
            [ (defaultNode "W" KGroup 0.0) { windowId = Just 1, title = "Outer", children = [ "A", "NW", "C", "B" ] }
            , (defaultNode "A" KTab 0.0) { parent = Just "W", tabId = Just 11, url = Just "http://a", title = "A" }
            , (defaultNode "NW" KGroup 0.0) { parent = Just "W", windowId = Just 2, title = "Inner", children = [ "X" ] }
            , (defaultNode "X" KTab 0.0) { parent = Just "NW", tabId = Just 21, url = Just "http://x", title = "X" }
            , (defaultNode "C" KTab 0.0) { parent = Just "W", tabId = Just 12, url = Just "http://c", title = "C" }
            , (defaultNode "B" KTab 0.0) { parent = Just "W", url = Just "http://b", title = "B" }
            ]
        , removes: []
        , roots: Just [ "W" ]
        }
        emptyModel
      activated = applyCommand 0.0 (Activate "B") m0
    activated.actions `shouldEqual` [ CreateTab (Just 1) (Just 2) (Just "http://b") ]

  -- The unification: a saved GROUP restores exactly like a saved window — its
  -- owning group goes live in place. Tab nesting is skipped when choosing that
  -- runtime group/window boundary.
  it "restoring a closed window with a nested group binds each tab to its own node" do
    let
      -- closed window n1 = [ A(n2), group n3 = [ B(n4) ], C(n5) ] — all closed, with urls
      m0 = applyPatch
        { upserts:
            [ (defaultNode "n1" KGroup 0.0) { title = "W", children = [ "n2", "n3", "n5" ] }
            , (defaultNode "n2" KTab 0.0) { parent = Just "n1", url = Just "http://a", title = "A" }
            , (defaultNode "n3" KGroup 0.0) { parent = Just "n1", title = "G", children = [ "n4" ] }
            , (defaultNode "n4" KTab 0.0) { parent = Just "n3", url = Just "http://b", title = "B" }
            , (defaultNode "n5" KTab 0.0) { parent = Just "n1", url = Just "http://c", title = "C" }
            ]
        , removes: []
        , roots: Just [ "n1" ]
        }
        emptyModel
      activated = applyCommand 0.0 (Activate "n1") m0
      -- window 5 reopens n1's own tabs (A, C); window 6 reopens the group's tab (B).
      -- the recreated tabs report redirected urls, so only window+order matching works.
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 5 }
        , openTabU 51 5 0 "http://a?x" "A"
        , openTabU 52 5 1 "http://c?x" "C"
        , WindowOpened { windowId: 6 }
        , openTabU 61 6 0 "http://b?x" "B"
        ]
    -- each closed node rebinds to its OWN recreated tab — C is not crossed with B
    (_.tabId <$> Map.lookup "n2" reopened.nodes) `shouldEqual` Just (Just 51) -- A
    (_.tabId <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just (Just 52) -- C
    (_.tabId <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just (Just 61) -- B, in the group's window
    (_.windowId <$> Map.lookup "n1" reopened.nodes) `shouldEqual` Just (Just 5)
    (_.windowId <$> Map.lookup "n3" reopened.nodes) `shouldEqual` Just (Just 6)

  it "restoring a saved group lights it up as a new window in place (group goes live)" do
    let
      grp = (defaultNode "g1" KGroup 0.0) { title = "Saved", children = [ "t1" ] }
      tab = (defaultNode "t1" KTab 0.0) { title = "T", url = Just "http://t", parent = Just "g1" }
      -- base.nextId is 4, so the imported group -> n4 and its tab -> n5
      saved = (applyCommand 0.0 (Import { nodes: [ grp, tab ], roots: [ "g1" ] }) base).model
      activated = applyCommand 0.0 (Activate "n4") saved
    -- a saved group is not a live window, so its tabs open as ONE new window
    -- (no bare CreateTab into the focused window)
    activated.actions `shouldEqual` [ CreateWindow [ "http://t" ] ]
    -- ...and the group node itself queues to bind that window — it goes live in place
    (map _.node activated.model.pendingRestoreWindows) `shouldEqual` [ "n4" ]
    let
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 5 }, openTab 71 5 0 "t" true ]
    -- the very same group node n4 is now a live window bound to browser window 5
    (isLive <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just true
    (_.windowId <$> Map.lookup "n4" reopened.nodes) `shouldEqual` Just (Just 5)
    -- its tab re-bound under it; no duplicate window node appeared
    (isLive <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just true
    (_.parent <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just (Just "n4")
    reopened.roots `shouldEqual` [ "n1", "n4" ]

  it "restoring ONE tab from a saved group rebinds that tab, not a sibling" do
    let
      grp = (defaultNode "g1" KGroup 0.0) { title = "Saved", children = [ "t1", "t2" ] }
      a = (defaultNode "t1" KTab 0.0) { title = "A", url = Just "http://a", parent = Just "g1" }
      b = (defaultNode "t2" KTab 0.0) { title = "B", url = Just "http://b", parent = Just "g1" }
      -- base.nextId is 4: import remaps g1 -> n4, t1 -> n5 (A), t2 -> n6 (B)
      saved = (applyCommand 0.0 (Import { nodes: [ grp, a, b ], roots: [ "g1" ] }) base).model
      -- restore only tab B (n6); its saved-group parent goes live as one new window
      activated = applyCommand 0.0 (Activate "n6") saved
    activated.actions `shouldEqual` [ CreateWindow [ "http://b" ] ]
    -- exactly B is queued to rebind in that window — NOT its sibling A (the bug:
    -- re-deriving "all the group's closed children" would queue [n5, n6])
    (map _.tabs activated.model.pendingRestoreWindows) `shouldEqual` [ Cons "n6" Nil ]
    let
      reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model
        [ WindowOpened { windowId: 5 }, openTab 71 5 0 "b" true ]
    -- B (n6) rebound to the created tab and flagged restored
    (_.tabId <$> Map.lookup "n6" reopened.nodes) `shouldEqual` Just (Just 71)
    (_.restoredFromClosed <$> Map.lookup "n6" reopened.nodes) `shouldEqual` Just true
    -- A (n5) is untouched: still closed, unflagged, identity (url) intact — not hijacked
    (isLive <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just false
    (_.restoredFromClosed <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just false
    (_.url <$> Map.lookup "n5" reopened.nodes) `shouldEqual` Just (Just "http://a")
    -- browser-closing the restored B keeps it as history (it belongs in the tree);
    -- A is likewise untouched
    let afterClose = (applyBrowser 0.0 (TabClosed { tabId: 71 }) reopened).model
    (isLive <$> Map.lookup "n6" afterClose.nodes) `shouldEqual` Just false
    (_.url <$> Map.lookup "n6" afterClose.nodes) `shouldEqual` Just (Just "http://b")
    (_.url <$> Map.lookup "n5" afterClose.nodes) `shouldEqual` Just (Just "http://a")

  it "property: one-tab restore from a saved group tolerates either window/tab event order" $
    quickCheck \(windowFirst :: Boolean) ->
      let
        grp = (defaultNode "g1" KGroup 0.0) { title = "Saved", children = [ "t1", "t2" ] }
        a = (defaultNode "t1" KTab 0.0) { title = "A", url = Just "http://a", parent = Just "g1" }
        b = (defaultNode "t2" KTab 0.0) { title = "B", url = Just "http://b", parent = Just "g1" }
        saved = (applyCommand 0.0 (Import { nodes: [ grp, a, b ], roots: [ "g1" ] }) base).model
        activated = applyCommand 0.0 (Activate "n6") saved
        win = WindowOpened { windowId: 5 }
        tab = openTab 71 5 0 "b" true
        events = if windowFirst then [ win, tab ] else [ tab, win ]
        reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model events
      in
        { groupWindow: _.windowId <$> Map.lookup "n4" reopened.nodes
        , restoredTab: _.tabId <$> Map.lookup "n6" reopened.nodes
        , siblingLive: isLive <$> Map.lookup "n5" reopened.nodes
        , pendingWindows: reopened.pendingRestoreWindows
        , pendingTabs: Map.lookup 5 reopened.pendingRestore
        , roots: reopened.roots
        , nodeCount: Map.size reopened.nodes
        }
          ===
            { groupWindow: Just (Just 5)
            , restoredTab: Just (Just 71)
            , siblingLive: Just false
            , pendingWindows: []
            , pendingTabs: Nothing
            , roots: [ "n1", "n4" ]
            , nodeCount: 6
            }

  it "property: multi-tab restore drains the queue regardless of the window event slot" $
    quickCheck \(rawSlot :: Int) ->
      let
        grp = (defaultNode "g1" KGroup 0.0) { title = "Saved", children = [ "t1", "t2" ] }
        a = (defaultNode "t1" KTab 0.0) { title = "A", url = Just "http://a", parent = Just "g1" }
        b = (defaultNode "t2" KTab 0.0) { title = "B", url = Just "http://b", parent = Just "g1" }
        saved = (applyCommand 0.0 (Import { nodes: [ grp, a, b ], roots: [ "g1" ] }) base).model
        activated = applyCommand 0.0 (Activate "n4") saved
        win = WindowOpened { windowId: 5 }
        tabA = openTab 71 5 0 "a" true
        tabB = openTab 72 5 1 "b" false
        slot = ((rawSlot `mod` 3) + 3) `mod` 3
        events =
          if slot == 0 then [ win, tabA, tabB ]
          else if slot == 1 then [ tabA, win, tabB ]
          else [ tabA, tabB, win ]
        reopened = foldl (\m e -> (applyBrowser 0.0 e m).model) activated.model events
      in
        { groupWindow: _.windowId <$> Map.lookup "n4" reopened.nodes
        , firstTab: _.tabId <$> Map.lookup "n5" reopened.nodes
        , secondTab: _.tabId <$> Map.lookup "n6" reopened.nodes
        , pendingWindows: reopened.pendingRestoreWindows
        , pendingTabs: Map.lookup 5 reopened.pendingRestore
        , roots: reopened.roots
        , nodeCount: Map.size reopened.nodes
        }
          ===
            { groupWindow: Just (Just 5)
            , firstTab: Just (Just 71)
            , secondTab: Just (Just 72)
            , pendingWindows: []
            , pendingTabs: Nothing
            , roots: [ "n1", "n4" ]
            , nodeCount: 6
            }

  it "property: one-by-one saved-group restores create tabs at saved live indices" $
    quickCheck \(raw :: Array Int) ->
      let
        order = restoreOrder raw
        restored = foldl restoreOne
          { model: savedGroupModel, browser: [], nextTab: 100, windowId: Nothing }
          order
      in
        { browser: restored.browser
        , model: liveChildIds restored.model "g"
        }
          ===
            { browser: restoreIds
            , model: restoreIds
            }

  it "property: user command sequences preserve live tab order through browser actions" $
    quickCheck \(raw :: Array (Array Int)) ->
      let
        final = flushAll (foldl simUserStep (settleCheck userSimInit) (Array.take 50 raw))
      in
        { order: final.failures, actions: final.actionFailures } === { order: [], actions: [] }

  -- The close rule: a browser-closed tab keeps its place as closed history ONLY if
  -- it was restored from history (it belongs in the tree) or the outliner itself
  -- closed it ("save & close"); a freshly-opened tab the user just closes is dropped,
  -- not auto-saved.
  describe "browser-close keeps only restored tabs" do
    -- A(n2) under window n1, saved as history, then restored and reopened as tab 99.
    -- `restored` has nodes n1 (live window) + n2 (restored A, tab 99); nextId = 3.
    let
      restored =
        let
          closed = outlinerClose "n2" 11 (runEvents [ openTab 11 1 0 "A" true ])
          activated = applyCommand 0.0 (Activate "n2") closed
        in
          (applyBrowser 0.0 (openTab 99 1 0 "A" true) activated.model).model

    it "keeps a restored tab as history on a browser close" do
      -- sanity: the reopened tab is flagged as restored
      (_.restoredFromClosed <$> Map.lookup "n2" restored.nodes) `shouldEqual` Just true
      let afterClose = (applyBrowser 0.0 (TabClosed { tabId: 99 }) restored).model
      -- kept as closed history under its window (it belongs in the tree)...
      (isLive <$> Map.lookup "n2" afterClose.nodes) `shouldEqual` Just false
      (_.children <$> Map.lookup "n1" afterClose.nodes) `shouldEqual` Just [ "n2" ]
      -- ...with the restore flag cleared now that it is closed again
      (_.restoredFromClosed <$> Map.lookup "n2" afterClose.nodes) `shouldEqual` Just false

    it "drops a freshly-opened tab on a browser close" do
      -- a brand-new tab (n3, tab 50) the user opens then closes, never restored
      let
        fresh = (applyBrowser 0.0 (openTab 50 1 1 "Fresh" false) restored).model
        afterClose = (applyBrowser 0.0 (TabClosed { tabId: 50 }) fresh).model
      -- the fresh tab is gone; the restored A (n2) and the window remain
      Map.lookup "n3" afterClose.nodes `shouldEqual` Nothing
      (_.tabId <$> Map.lookup "n2" afterClose.nodes) `shouldEqual` Just (Just 99)
      (_.children <$> Map.lookup "n1" afterClose.nodes) `shouldEqual` Just [ "n2" ]

    it "keeps a fresh tab when the outliner closes it (save & close)" do
      -- the same fresh tab, closed via the outliner, IS kept — an explicit save,
      -- unlike a browser close of an identical tab
      let
        fresh = (applyBrowser 0.0 (openTab 50 1 1 "Fresh" false) restored).model
        closing = applyCommand 0.0 (CloseNode "n3") fresh
        afterClose = (applyBrowser 0.0 (TabClosed { tabId: 50 }) closing.model).model
      closing.actions `shouldEqual` [ RemoveTab 50 ]
      Set.member 50 closing.model.closingTabs `shouldEqual` true
      (isLive <$> Map.lookup "n3" afterClose.nodes) `shouldEqual` Just false
      (_.children <$> Map.lookup "n1" afterClose.nodes) `shouldEqual` Just [ "n2", "n3" ]
      Set.member 50 afterClose.closingTabs `shouldEqual` false

  -- Dragging a LIVE tab to a new owning container drives the real browser tab; the
  -- tree is left untouched and re-settles from the resulting onAttached/onCreated.
  describe "live-tab moves drive the browser" do
    it "into another live window: moves the real tab there at the dropped index" do
      let r = applyCommand 0.0 (Move "n2" (Just "n4") 1) base2 -- n2 (tab 11) -> window n4 (id 2), index 1
      r.actions `shouldEqual` [ MoveTabToWindow 11 2 1 ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1") -- unchanged until events
      r.model.pendingRestoreWindows `shouldEqual` []

    it "into a saved group: the group goes live as a new window (queued to rebind)" do
      let
        withGroup = (applyCommand 0.0 (NewGroup Nothing 0) base2).model -- group n6 at root
        r = applyCommand 0.0 (Move "n2" (Just "n6") 0) withGroup
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      -- n6 binds when its window opens; it carries no tabs to rebind (the dragged
      -- live tab arrives via onAttached, and n6's own saved tabs stay put)
      (map _.node r.model.pendingRestoreWindows) `shouldEqual` [ "n6" ]
      (map _.tabs r.model.pendingRestoreWindows) `shouldEqual` [ Nil ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1")

    it "property: live-tab rehome to a saved group tolerates either window/attach event order" $
      quickCheck \(windowFirst :: Boolean) ->
        let
          withGroup = (applyCommand 0.0 (NewGroup Nothing 0) base2).model -- group n6 at root
          r = applyCommand 0.0 (Move "n2" (Just "n6") 0) withGroup
          win = WindowOpened { windowId: 5 }
          attach = TabAttached { tabId: 11, windowId: 5, index: 0 }
          events = if windowFirst then [ win, attach ] else [ attach, win ]
          moved = foldl (\m e -> (applyBrowser 0.0 e m).model) r.model events
        in
          { groupWindow: _.windowId <$> Map.lookup "n6" moved.nodes
          , movedParent: _.parent <$> Map.lookup "n2" moved.nodes
          , pendingWindows: moved.pendingRestoreWindows
          , roots: moved.roots
          , nodeCount: Map.size moved.nodes
          }
            ===
              { groupWindow: Just (Just 5)
              , movedParent: Just (Just "n6")
              , pendingWindows: []
              , roots: [ "n6", "n1", "n4" ]
              , nodeCount: 6
              }

    it "out to the root: detaches into a brand-new window" do
      let r = applyCommand 0.0 (Move "n2" Nothing 0) base2
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      r.model.pendingRestoreWindows `shouldEqual` [] -- a fresh window node appears via onCreated
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1")

    it "within its own window: moves the real browser tab" do
      let r = applyCommand 0.0 (Move "n3" (Just "n1") 0) base2
      r.actions `shouldEqual` [ MoveTabToWindow 12 1 0 ]
      (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "n2", "n3" ]
      let moved = (applyBrowser 0.0 (TabMoved { tabId: 12, windowId: 1, toIndex: 0 }) r.model).model
      (_.children <$> Map.lookup "n1" moved.nodes) `shouldEqual` Just [ "n3", "n2" ]

    it "drop before a live tab skips interleaved closed rows when choosing the browser index" do
      let
        closed = (defaultNode "nx" KTab 0.0) { parent = Just "n1", url = Just "http://x", title = "X", closedAt = Just 0.0 }
        m = case Map.lookup "n1" base.nodes of
          Just w ->
            applyPatch
              { upserts: [ w { children = [ "nx", "n2", "n3" ] }, closed ]
              , removes: []
              , roots: Nothing
              }
              base
          Nothing -> base
        r = applyCommand 0.0 (Drop "n3" "n2") m
      r.actions `shouldEqual` [ MoveTabToWindow 12 1 0 ]
      (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "nx", "n2", "n3" ]
      let moved = (applyBrowser 0.0 (TabMoved { tabId: 12, windowId: 1, toIndex: 0 }) r.model).model
      (_.children <$> Map.lookup "n1" moved.nodes) `shouldEqual` Just [ "nx", "n3", "n2" ]

  -- "Move to top level" pulls a nested node out to the root just after the root it
  -- belongs to; "Move to bottom" sends it to the very end. A non-live node moves
  -- purely in the tree; a live tab is promoted into its own new window.
  describe "move to top level / bottom" do
    -- two saved top-level groups: R1 = [ A, G=[B] ] and R2 = [ C ] — all closed
    let
      closed = applyPatch
        { upserts:
            [ (defaultNode "R1" KGroup 0.0) { title = "R1", children = [ "A", "G" ] }
            , (defaultNode "A" KTab 0.0) { parent = Just "R1", url = Just "http://a", title = "A" }
            , (defaultNode "G" KGroup 0.0) { parent = Just "R1", title = "G", children = [ "B" ] }
            , (defaultNode "B" KTab 0.0) { parent = Just "G", url = Just "http://b", title = "B" }
            , (defaultNode "R2" KGroup 0.0) { title = "R2", children = [ "C" ] }
            , (defaultNode "C" KTab 0.0) { parent = Just "R2", url = Just "http://c", title = "C" }
            ]
        , removes: []
        , roots: Just [ "R1", "R2" ]
        }
        emptyModel

    it "move to top level pulls a nested node out, just after its root ancestor (tab wrapped)" do
      let r = applyCommand 0.0 (MoveTopLevel "B") closed
      -- B is a tab, which can't sit bare at the root, so it lands wrapped in a fresh
      -- group at index 1 (right after R1), not at the very end
      r.model.roots `shouldEqual` [ "R1", "n1", "R2" ]
      (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "B" ]
      (_.parent <$> Map.lookup "B" r.model.nodes) `shouldEqual` Just (Just "n1")
      -- pulling out G's only child prunes the now-empty group; R1 keeps its other child
      Map.lookup "G" r.model.nodes `shouldEqual` Nothing
      (_.children <$> Map.lookup "R1" r.model.nodes) `shouldEqual` Just [ "A" ]
      r.actions `shouldEqual` [] -- tree-only, never touches the browser

    it "move to bottom pulls a nested node to the very end of the root list (tab wrapped)" do
      let r = applyCommand 0.0 (MoveBottom "B") closed
      r.model.roots `shouldEqual` [ "R1", "R2", "n1" ]
      (_.children <$> Map.lookup "n1" r.model.nodes) `shouldEqual` Just [ "B" ]
      (_.parent <$> Map.lookup "B" r.model.nodes) `shouldEqual` Just (Just "n1")
      Map.lookup "G" r.model.nodes `shouldEqual` Nothing
      r.actions `shouldEqual` []

    it "a tab wrapped at the root restores through its group (flagged), not into the current window" do
      -- move closed tab B to the root: it wraps in group n1. Restoring B now routes via
      -- that group (a new window) and flags it, so a later browser close KEEPS it —
      -- closing the parentless-root-tab gap (a bare root tab would reopen unflagged).
      let
        wrapped = run (MoveTopLevel "B") closed
        activated = applyCommand 0.0 (Activate "B") wrapped
      activated.actions `shouldEqual` [ CreateWindow [ "http://b" ] ]
      (map _.tabs activated.model.pendingRestoreWindows) `shouldEqual` [ Cons "B" Nil ]
      (_.restoredFromClosed <$> Map.lookup "B" activated.model.nodes) `shouldEqual` Just true

    -- wrapRootTabsModel is the shared enforcer (used per-command and at boot to
    -- normalize loaded data). It wraps a bare root CLOSED tab, but leaves a live tab
    -- (transient at root during a move/flatten) and a container alone.
    it "wrapRootTabsModel wraps a bare root closed tab only" do
      let
        m = applyPatch
          { upserts:
              [ (defaultNode "ct" KTab 0.0) { url = Just "http://c", title = "C", closedAt = Just 0.0 }
              , (defaultNode "lt" KTab 0.0) { tabId = Just 9, url = Just "http://l", title = "L" }
              , (defaultNode "grp" KGroup 0.0) { title = "G" }
              ]
          , removes: []
          , roots: Just [ "ct", "lt", "grp" ]
          }
          emptyModel
        w = wrapRootTabsModel 0.0 m
      -- closed tab "ct" wrapped in a fresh group "n1"; live "lt" and group "grp" untouched
      w.model.roots `shouldEqual` [ "n1", "lt", "grp" ]
      (_.children <$> Map.lookup "n1" w.model.nodes) `shouldEqual` Just [ "ct" ]
      (_.parent <$> Map.lookup "ct" w.model.nodes) `shouldEqual` Just (Just "n1")
      (_.parent <$> Map.lookup "lt" w.model.nodes) `shouldEqual` Just Nothing

    it "move to bottom reorders a non-last top-level node to the end" do
      let m = run (MoveBottom "R1") closed
      m.roots `shouldEqual` [ "R2", "R1" ]

    it "move to top level is a no-op on a node already at the top level" do
      (run (MoveTopLevel "R1") closed).roots `shouldEqual` [ "R1", "R2" ]

    it "move to bottom is a no-op on the last top-level node" do
      (run (MoveBottom "R2") closed).roots `shouldEqual` [ "R1", "R2" ]

    -- A live tab can't sit bare at the root, so promoting one detaches the REAL tab
    -- into its own new window (exactly like dragging it to the root); the tree is
    -- left untouched until the resulting browser events arrive.
    it "move to top level on a live tab promotes it into its own new window" do
      let r = applyCommand 0.0 (MoveTopLevel "n2") base -- n2 is a live tab (tab 11) in window n1
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1") -- unchanged until events

    it "move to bottom on a live tab promotes it into its own new window" do
      let r = applyCommand 0.0 (MoveBottom "n2") base
      r.actions `shouldEqual` [ NewWindowWithTabs [ 11 ] ]
      (_.parent <$> Map.lookup "n2" r.model.nodes) `shouldEqual` Just (Just "n1")

  -- An emptied container is clutter, so it's pruned — unless the user renamed it,
  -- which marks it as a deliberate label worth keeping.
  describe "pruning emptied groups" do
    -- group n4 at root containing a child group n5 (base.nextId is 4)
    let nested = run (NewGroup (Just "n4") 0) (run (NewGroup Nothing 0) base)

    it "moving a group's last child out prunes the now-empty group" do
      let m = run (Move "n5" Nothing 0) nested
      Map.lookup "n4" m.nodes `shouldEqual` Nothing
      (_.parent <$> Map.lookup "n5" m.nodes) `shouldEqual` Just Nothing
      m.roots `shouldEqual` [ "n5", "n1" ]

    it "deleting a group's last child prunes the now-empty group" do
      let m = run (Delete "n5") nested
      Map.lookup "n5" m.nodes `shouldEqual` Nothing
      Map.lookup "n4" m.nodes `shouldEqual` Nothing

    it "a renamed group emptied of children is kept" do
      let m = run (Move "n5" Nothing 0) (run (Rename "n4" "Keep") nested)
      (_.customTitle <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just "Keep")
      (_.children <$> Map.lookup "n4" m.nodes) `shouldEqual` Just []

    it "pruning cascades up, stopping at a renamed ancestor" do
      let
        deep = run (NewGroup (Just "n5") 0) nested -- group n6 inside n5 inside n4
        renamed = run (Rename "n4" "Keep") deep -- keep the outer group
        m = run (Move "n6" Nothing 0) renamed -- empty n5 -> prune n5 -> n4 empty but kept
      Map.lookup "n5" m.nodes `shouldEqual` Nothing
      (_.customTitle <$> Map.lookup "n4" m.nodes) `shouldEqual` Just (Just "Keep")
      (_.children <$> Map.lookup "n4" m.nodes) `shouldEqual` Just []
