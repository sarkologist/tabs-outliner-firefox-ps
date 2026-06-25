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
import Effect.Profile as Profile
import Model.Codec (encodeSnapshot)
import Model.Command (BrowserAction(..), Request(..), applyCommand, decodeRequest)
import Model.Event (BrowserEvent)
import Model.Reconcile (applyBrowser)
import Model.Rematch (rematchOnStartup)
import Model.Types (Patch)
import Model.Undo (applyEntry, inversePatch, undoable)
import Model.View (OrderEntry, computeOrder, encodeView, focusIndexOf, sliceView)

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
  ref <- liftEffect (Ref.new rematched.model)
  -- The model's structural version: bumped on every change so the sidebar's view
  -- cache knows when its visible order is stale. The cache memoizes the order for
  -- the last (version, query), so scrolling (same query, no edits) is O(window).
  versionRef <- liftEffect (Ref.new 0)
  viewRef <- liftEffect (Ref.new { version: (-1), query: "", order: ([] :: Array OrderEntry) })
  -- persist the re-match for durability, but do NOT broadcast yet: we aren't
  -- serving requests until onRequest is registered below, and the sidebar whose
  -- message woke this (possibly suspended) page must hear its refetch ping only
  -- once we can answer it.
  unless (isEmptyPatch rematched.patch) (Persist.writePatch db rematched.patch)
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
      persistAndBroadcast api db versionRef s.patch

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
          persistAndBroadcast api db versionRef a.patch
          traverse_ (runAction api) a.actions
          pure ackJson

  -- Live browser events.
  liftEffect $ Browser.subscribe api \ev -> launchAff_ (dispatch ev)

  -- Serve the sidebar. A command applies, persists, bumps the version, broadcasts
  -- `invalidate`, and runs its browser actions; a GetView returns one window of the
  -- visible order. The sidebar holds no model — it only ever renders the window it
  -- is given here.
  liftEffect $ Channel.onRequest api \reqJson -> case decodeRequest reqJson of
    Right (GetView vr) -> do
      ts0 <- liftEffect Profile.nowMs
      m <- liftEffect (Ref.read ref)
      v <- liftEffect (Ref.read versionRef)
      cache <- liftEffect (Ref.read viewRef)
      -- reuse the cached order unless the model changed or the query differs;
      -- a pure scroll (same version + query) skips the O(N) recompute.
      order <-
        if cache.version == v && cache.query == vr.query then pure cache.order
        else do
          let o = computeOrder vr.query m
          liftEffect (Ref.write { version: v, query: vr.query, order: o } viewRef)
          pure o
      let
        focusIndex = case vr.myWindow of
          Just w | vr.wantFocus -> focusIndexOf w order m
          _ -> -1
        rows = sliceView m order vr.start vr.count
        total = Array.length order
      ts1 <- liftEffect Profile.nowMs
      pure (encodeView { total, rows, focusIndex, serverMs: ts1 - ts0 })
    Right (RunCommand cmd) -> do
      m <- liftEffect (Ref.read ref)
      t <- liftEffect nowMs
      let r = applyCommand t cmd m
      liftEffect do
        Ref.write r.model ref
        -- record the inverse so this command can be undone; a fresh edit
        -- invalidates any redo future. A command that relocated real browser tabs
        -- (a live-tab move, or flattening a live window) is skipped: undo can't move
        -- those tabs back, so reverting only the tree would desync the two.
        when (undoable cmd && not (isEmptyPatch r.patch) && not (Array.any relocates r.actions)) do
          Ref.modify_ (pushBounded (inversePatch t m r.patch)) undoRef
          Ref.write [] redoRef
      persistAndBroadcast api db versionRef r.patch
      traverse_ (runAction api) r.actions
      pure ackJson
    Right Undo -> stepStack undoRef redoRef
    Right Redo -> stepStack redoRef undoRef
    -- export needs the whole tree; it's a rare, explicit user action, so paying
    -- O(total) once here (rather than keeping a model copy in the sidebar) is fine.
    Right Export -> do
      m <- liftEffect (Ref.read ref)
      pure (encodeSnapshot m)
    _ -> pure ackJson

  -- We can now serve requests: ping every open sidebar to (re)fetch its window.
  -- UNCONDITIONAL — on a clean restart the re-match patch is empty, but a sidebar
  -- that opened while this (event) page was suspended still needs this wake-up to
  -- recover from its first request racing our boot.
  liftEffect (Channel.broadcast api invalidateMsg)

-- Bump the structural version, ping open sidebars to re-fetch, then persist.
-- The version bump + broadcast are synchronous with the caller's model write (no
-- await between), so a GetView can never observe the new model against a stale
-- cached order, and a slow/failed persist can't strand the sidebar on stale rows
-- (it re-fetches from the in-memory model, not IndexedDB). No-op patches are
-- skipped entirely. Persist is last, purely for durability across a bg restart.
persistAndBroadcast :: BrowserApi -> Persist.Db -> Ref.Ref Int -> Patch -> Aff Unit
persistAndBroadcast api db versionRef patch = unless (isEmptyPatch patch) do
  liftEffect do
    Ref.modify_ (_ + 1) versionRef
    Channel.broadcast api invalidateMsg
  Persist.writePatch db patch

invalidateMsg :: Json
invalidateMsg = encodeJson { invalidate: true }

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
  MoveTabToWindow t w i -> Browser.moveTabToWindow api t w i
  NewWindowWithTabs ts -> Browser.newWindowWithTabs api ts
  RemoveTab t -> Browser.removeTab api t

-- | Did this command relocate real browser tabs (move them between windows or into
-- | a new one)? Such a command can't be undone — undo reverts only the tree, not
-- | the browser — so the background skips recording an undo entry for it.
relocates :: BrowserAction -> Boolean
relocates (MoveTabToWindow _ _ _) = true
relocates (NewWindowWithTabs _) = true
relocates _ = false
