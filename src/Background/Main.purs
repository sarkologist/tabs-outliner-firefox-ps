-- | The background owner: the only context that observes browser events and the
-- | only writer of the model + IndexedDB. Boot loads the persisted forest, seeds
-- | the current live windows, then every event/command produces a patch that is
-- | persisted (O(change)) and broadcast to any open sidebar.
module Background.Main where

import Prelude

import Data.Array (concatMap, fromFoldable)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.DateTime.Instant (unInstant)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Browser (RuntimeWindow)
import Effect.Browser as Browser
import Effect.Channel as Channel
import Effect.Persist as Persist
import Model.Codec (encodePatch, encodeSnapshot)
import Model.Event (BrowserEvent(..))
import Model.Reconcile (applyBrowser)

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
    dispatch :: BrowserEvent -> Aff Unit
    dispatch ev = do
      t <- liftEffect nowMs
      m <- liftEffect (Ref.read ref)
      let s = applyBrowser t ev m
      liftEffect (Ref.write s.model ref)
      Persist.writePatch db s.patch
      liftEffect (Channel.broadcast api (encodePatch s.patch))

  -- Live browser events.
  liftEffect $ Browser.subscribe api \ev -> launchAff_ (dispatch ev)

  -- Serve the sidebar's one-shot snapshot request. (Commands join here in M4.)
  liftEffect $ Channel.onRequest api \_req -> do
    m <- liftEffect (Ref.read ref)
    pure (encodeSnapshot m)
