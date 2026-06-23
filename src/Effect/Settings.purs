-- | localStorage-backed store for the keyboard-shortcut overrides, shared by the
-- | sidebar and the options page: both are pages of the same extension origin, so
-- | they share one localStorage (and writes fire a "storage" event in the other).
-- | Overrides are a flat { commandKey: combo } JSON object; anything missing falls
-- | back to Model.Shortcuts.defaultBinding. This module also owns the keydown
-- | plumbing, so the canonical-combo string is produced in exactly one place.
module Effect.Settings
  ( getShortcuts
  , setShortcuts
  , onShortcut
  , captureCombo
  ) where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)

-- | Read the stored overrides ({} when unset or corrupt).
foreign import getShortcuts :: Effect (Object String)

-- | Persist the overrides (also notifies other extension pages via "storage").
foreign import setShortcuts :: Object String -> Effect Unit

-- | Install a document-level keydown handler. The callback receives the canonical
-- | combo string and returns whether it consumed the event; when it does, the JS
-- | calls preventDefault. Keystrokes while a text input / the rename box is
-- | focused are ignored and never reach the callback.
foreign import onShortcut :: (String -> Effect Boolean) -> Effect Unit

-- | One-shot: capture the next keypress and hand back its canonical combo (lone
-- | modifier presses are skipped; the event is swallowed). Drives the options
-- | page's "record a shortcut" button. Escape yields the combo "Escape".
foreign import captureCombo :: (String -> Effect Unit) -> Effect Unit
