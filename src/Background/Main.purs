-- | The background owner: the only context that observes browser events and the
-- | only writer of the model + IndexedDB. Boot loads the persisted forest, seeds
-- | the current live windows, then every event/command produces a patch that is
-- | persisted (O(change)) and broadcast to any open sidebar.
module Background.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (concatMap, fromFoldable)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Browser (BrowserApi, RuntimeWindow)
import Effect.Browser as Browser
import Effect.Channel as Channel
import Effect.Persist as Persist
import Model.Codec (encodePatch, encodeSnapshot)
import Model.Command (BrowserAction(..), Request(..), applyCommand, decodeRequest)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)
import Model.Types (Patch)

nowMs :: Effect Number
nowMs = (unwrap <<< unInstant) <$> now

windowEvents :: RuntimeWindow -> Array BrowserEvent
windowEvents w = [ WindowOpened { windowId: w.windowId } ] <> map tabEvent w.tabs
  where
  tabEvent t = TabOpened
    { tabId: t.tabId
    , windowId: t.windowId
    , index: t.index
    , url: t.url
    , title: t.title
    , active: t.active
    , favIconUrl: t.favIconUrl
    }

main :: Effect Unit
main = launchAff_ do
  api <- liftEffect Browser.getBrowser
  db <- Persist.open
  loaded <- Persist.load db
  t0 <- liftEffect nowMs
  wins <- Browser.getAllWindows api

  -- Boot: persisted forest + a fresh mirror of the live windows. (M5 replaces
  -- the naive seed with a URL re-match against the persisted live nodes.)
  let
    model0 = Persist.modelFromLoaded loaded.nodes loaded.roots
    seeded = foldl (\m ev -> (applyBrowser t0 ev m).model) model0 (concatMap windowEvents wins)
    bootPatch = { upserts: fromFoldable (Map.values seeded.nodes), removes: [], roots: Just seeded.roots }
  Persist.writePatch db bootPatch
  ref <- liftEffect (Ref.new seeded)

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

  -- Live browser events.
  liftEffect $ Browser.subscribe api \ev -> launchAff_ (dispatch ev)

  -- Serve the sidebar: snapshot requests and commands. A command applies,
  -- persists, broadcasts the patch to every sidebar, and runs its browser
  -- actions (focus/create/remove).
  liftEffect $ Channel.onRequest api \reqJson -> do
    m <- liftEffect (Ref.read ref)
    case decodeRequest reqJson of
      Right GetSnapshot -> pure (encodeSnapshot m)
      Right (RunCommand cmd) -> do
        t <- liftEffect nowMs
        let r = applyCommand t cmd m
        liftEffect (Ref.write r.model ref)
        persistAndBroadcast api db r.patch
        traverse_ (runAction api) r.actions
        pure ackJson
      Left _ -> pure (encodeSnapshot m)

-- Persist + broadcast a patch, skipping the no-op patches that focus/close/
-- restore commands and ignored events produce (no IDB tx, no message).
persistAndBroadcast :: BrowserApi -> Persist.Db -> Patch -> Aff Unit
persistAndBroadcast api db patch = unless (isEmptyPatch patch) do
  Persist.writePatch db patch
  liftEffect (Channel.broadcast api (encodePatch patch))

isEmptyPatch :: Patch -> Boolean
isEmptyPatch p = Array.null p.upserts && Array.null p.removes && isNothing p.roots

ackJson :: Json
ackJson = encodeJson { ok: true }

runAction :: BrowserApi -> BrowserAction -> Aff Unit
runAction api = case _ of
  FocusTab t -> Browser.focusTab api t
  CreateTab w u -> Browser.createTab api w u
  RemoveTab t -> Browser.removeTab api t
