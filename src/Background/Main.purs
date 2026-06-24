-- | The background owner: the only context that observes browser events and the
-- | only writer of the model + IndexedDB. Boot loads the persisted forest and
-- | URL-rematches it against the live windows, then every event/command produces
-- | a patch that is persisted (O(change)) and broadcast to any open sidebar.
module Background.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Browser (BrowserApi)
import Effect.Browser as Browser
import Effect.Channel as Channel
import Effect.Persist as Persist
import Model.Codec (encodePatch)
import Model.Command (BrowserAction(..), Request(..), applyCommand, decodeRequest)
import Model.Event (BrowserEvent)
import Model.Reconcile (applyBrowser)
import Model.Rematch (rematchOnStartup)
import Model.Types (Patch)
import Model.Undo (applyEntry, inversePatch, undoable)

nowMs :: Effect Number
nowMs = (unwrap <<< unInstant) <$> now

main :: Effect Unit
main = launchAff_ do
  api <- liftEffect Browser.getBrowser
  db <- Persist.open
  loaded <- Persist.load db
  t0 <- liftEffect nowMs
  wins <- Browser.getAllWindows api

  -- Boot: re-bind reopened tabs to their existing nodes by url; the patch
  -- touches only changed nodes, so subsequent boots are O(change).
  let
    model0 = Persist.modelFromLoaded loaded.nodes loaded.roots
    rematched = rematchOnStartup t0 wins model0
  -- broadcast too, so a sidebar already open at startup gets the re-match
  -- (it loads the rest straight from IndexedDB)
  persistAndBroadcast api db rematched.patch
  ref <- liftEffect (Ref.new rematched.model)
  -- Undo/redo are background-only state (not part of the shared, persisted Model):
  -- stacks of inverse patches. Browser events never touch them — you don't undo a
  -- tab the browser opened — so they survive arbitrary live activity between a
  -- command and its undo.
  undoRef <- liftEffect (Ref.new ([] :: Array Patch))
  redoRef <- liftEffect (Ref.new ([] :: Array Patch))

  let
    -- ATOMICITY: read -> applyX -> write is fully synchronous (no `await`
    -- between them), and Aff fibers are cooperatively scheduled, so two
    -- concurrent inputs can never interleave their read/compute/write. The
    -- first suspension point is the persist below. Keep it that way.
    dispatch :: BrowserEvent -> Aff Unit
    dispatch ev = do
      t <- liftEffect nowMs
      m <- liftEffect (Ref.read ref)
      let s = applyBrowser t ev m
      liftEffect (Ref.write s.model ref)
      persistAndBroadcast api db s.patch

    -- Undo/redo step: pop one inverse patch off `from`, apply it (reusing the
    -- command persist/broadcast path), and push the resulting inverse onto `to`.
    -- An empty stack is a no-op, so the sidebar can fire these unconditionally.
    stepStack :: Ref.Ref (Array Patch) -> Ref.Ref (Array Patch) -> Aff Json
    stepStack from to = do
      entries <- liftEffect (Ref.read from)
      case Array.uncons entries of
        Nothing -> pure ackJson
        Just { head: entry, tail } -> do
          m <- liftEffect (Ref.read ref)
          t <- liftEffect nowMs
          let a = applyEntry t entry m
          liftEffect do
            Ref.write a.model ref
            Ref.write tail from
            Ref.modify_ (pushBounded a.inverse) to
          persistAndBroadcast api db a.patch
          traverse_ (runAction api) a.actions
          pure ackJson

  -- Live browser events.
  liftEffect $ Browser.subscribe api \ev -> launchAff_ (dispatch ev)

  -- Serve the sidebar's commands. A command applies, persists, broadcasts the
  -- patch to every sidebar, and runs its browser actions (focus/create/remove).
  -- (The sidebar loads its initial model from IndexedDB directly, so there is no
  -- snapshot request.)
  liftEffect $ Channel.onRequest api \reqJson -> case decodeRequest reqJson of
    Right (RunCommand cmd) -> do
      m <- liftEffect (Ref.read ref)
      t <- liftEffect nowMs
      let r = applyCommand t cmd m
      liftEffect do
        Ref.write r.model ref
        -- record the inverse so this command can be undone; a fresh edit
        -- invalidates any redo future
        when (undoable cmd && not (isEmptyPatch r.patch)) do
          Ref.modify_ (pushBounded (inversePatch t m r.patch)) undoRef
          Ref.write [] redoRef
      persistAndBroadcast api db r.patch
      traverse_ (runAction api) r.actions
      pure ackJson
    Right Undo -> stepStack undoRef redoRef
    Right Redo -> stepStack redoRef undoRef
    _ -> pure ackJson

-- Persist + broadcast a patch, skipping the no-op patches that focus/close/
-- restore commands and ignored events produce (no IDB tx, no message).
persistAndBroadcast :: BrowserApi -> Persist.Db -> Patch -> Aff Unit
persistAndBroadcast api db patch = unless (isEmptyPatch patch) do
  Persist.writePatch db patch
  liftEffect (Channel.broadcast api (encodePatch patch))

isEmptyPatch :: Patch -> Boolean
isEmptyPatch p = Array.null p.upserts && Array.null p.removes && isNothing p.roots

-- | Cap on undo depth: bounds the inverse-patch memory a long session can
-- | accumulate (e.g. repeated large imports), dropping the oldest entry.
maxUndoDepth :: Int
maxUndoDepth = 50

pushBounded :: forall a. a -> Array a -> Array a
pushBounded x xs = Array.take maxUndoDepth (Array.cons x xs)

ackJson :: Json
ackJson = encodeJson { ok: true }

runAction :: BrowserApi -> BrowserAction -> Aff Unit
runAction api = case _ of
  FocusTab t -> Browser.focusTab api t
  CreateTab w u -> Browser.createTab api w u
  CreateWindow us -> Browser.createWindow api us
  MoveTabToWindow t w -> Browser.moveTabToWindow api t w
  NewWindowWithTab t -> Browser.newWindowWithTab api t
  RemoveTab t -> Browser.removeTab api t
