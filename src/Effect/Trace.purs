-- | Temporary debug tracing for diagnosing the restore flow end-to-end: the
-- | command plan, the browser actions, the raw events, and — crucially — the
-- | reducer's bind-vs-fresh decision (read off the emitted patch) plus the
-- | restore queues before/after. Pure formatters here; the `trace` sink is a
-- | flag-gated console.log (see Trace.js). Remove once the bug is pinned.
module Effect.Trace
  ( trace
  , getEnabled
  , setEnabled
  , readTrace
  , clearTrace
  , downloadTrace
  , fmtEvent
  , fmtPatch
  , fmtQueues
  , fmtCommand
  ) where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Model.Command (Command(..))
import Model.Event (BrowserEvent(..))
import Model.Types (Model, Node, Patch)

foreign import traceImpl :: String -> Effect Unit

-- | Is tracing enabled? (the localStorage flag the options page toggles).
foreign import getEnabled :: Effect Boolean

-- | Turn restore tracing on/off (from the options page).
foreign import setEnabled :: Boolean -> Effect Unit

-- | The whole captured trace buffer (newline-joined), for the options page to show.
foreign import readTrace :: Effect String

-- | Wipe the trace buffer.
foreign import clearTrace :: Effect Unit

-- | Download the trace buffer as a .txt (for sharing).
foreign import downloadTrace :: Effect Unit

-- | Emit one trace line (buffered + console-logged, flag-gated, in Trace.js).
trace :: String -> Effect Unit
trace = traceImpl

fmtMaybeStr :: Maybe String -> String
fmtMaybeStr = maybe "-" identity

fmtEvent :: BrowserEvent -> String
fmtEvent = case _ of
  WindowOpened { windowId } -> "WindowOpened win=" <> show windowId
  WindowClosed { windowId } -> "WindowClosed win=" <> show windowId
  TabOpened t ->
    "TabOpened tab=" <> show t.tabId <> " win=" <> show t.windowId
      <> " idx=" <> show t.index
      <> " active=" <> show t.active
      <> " url=" <> fmtMaybeStr t.url
  TabClosed { tabId } -> "TabClosed tab=" <> show tabId
  TabChanged c -> "TabChanged tab=" <> show c.tabId <> " url=" <> fmtMaybeStr c.url
  TabActivated a -> "TabActivated tab=" <> show a.tabId <> " win=" <> show a.windowId
  TabMoved m -> "TabMoved tab=" <> show m.tabId <> " win=" <> show m.windowId <> " toIdx=" <> show m.toIndex
  TabAttached a -> "TabAttached tab=" <> show a.tabId <> " win=" <> show a.windowId <> " idx=" <> show a.index

-- | One node, compact: id, kind, live bindings, parent, closed-ness. This is what
-- | distinguishes "bound an existing node" (a known id goes live) from "minted a
-- | fresh one" (a new id appears).
fmtNode :: Node -> String
fmtNode n = n.id <> "{" <> show n.kind <> tabS <> winS <> parS <> closedS <> "}"
  where
  tabS = maybe "" (\t -> " tab=" <> show t) n.tabId
  winS = maybe "" (\w -> " win=" <> show w) n.windowId
  parS = " par=" <> fmtMaybeStr n.parent
  closedS = maybe "" (const " CLOSED") n.closedAt

fmtPatch :: Patch -> String
fmtPatch p =
  "upserts=[" <> joinWith ", " (map fmtNode p.upserts) <> "]"
    <> (if Array.null p.removes then "" else " removes=[" <> joinWith "," p.removes <> "]")
    <> case p.roots of
      Nothing -> ""
      Just rs -> " rootsSet=[" <> joinWith "," rs <> "]"

-- | The two restore queues. `pendingWins` holds container nodes awaiting a new
-- | browser window to bind to; `pendingTabs` maps a (live) windowId to the tab
-- | nodes queued to rebind in it.
fmtQueues :: Model -> String
fmtQueues m =
  "pendingWins=[" <> joinWith "," m.pendingRestoreWindows <> "]"
    <> " pendingTabs={"
    <> joinWith ", " (map entry (Map.toUnfoldable m.pendingRestore :: Array (Tuple Int (List String))))
    <> "}"
  where
  entry (Tuple w ns) = show w <> ":[" <> joinWith "," (Array.fromFoldable ns) <> "]"

fmtCommand :: Command -> String
fmtCommand = case _ of
  Collapse nid v -> "Collapse " <> nid <> " " <> show v
  Rename nid t -> "Rename " <> nid <> " " <> show t
  Activate nid -> "Activate " <> nid
  CloseNode nid -> "CloseNode " <> nid
  Delete nid -> "Delete " <> nid
  Move nid mp i -> "Move " <> nid <> " -> " <> fmtMaybeStr mp <> " @" <> show i
  MoveTopLevel nid -> "MoveTopLevel " <> nid
  MoveBottom nid -> "MoveBottom " <> nid
  Flatten nid -> "Flatten " <> nid
  NewGroup mp i -> "NewGroup " <> fmtMaybeStr mp <> " @" <> show i
  Import snap -> "Import (" <> show (Array.length snap.nodes) <> " nodes)"
  Drop d t -> "Drop " <> d <> " -> " <> t
