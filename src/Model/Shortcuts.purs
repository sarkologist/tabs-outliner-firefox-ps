-- | Pure model for the configurable keyboard shortcuts: the commands a shortcut
-- | can trigger, their stable storage keys, human labels, default key combos,
-- | and the canonical-string matching shared by the sidebar (which dispatches on
-- | a keypress) and the options page (which displays/records bindings).
-- |
-- | A "combo" is a canonical string like "n", "/", "Shift+n" or "Ctrl+Alt+k":
-- | the active modifiers in the fixed order Ctrl, Alt, Shift, Meta, joined to the
-- | (lower-cased, single-char) key with "+". The JS in Effect.Settings builds the
-- | exact same string from a KeyboardEvent, so matching is a plain string compare
-- | and these defaults must be written in that same canonical form.
module Model.Shortcuts
  ( Cmd(..)
  , allCmds
  , keyOf
  , labelOf
  , defaultBinding
  , bindingFor
  , cmdForCombo
  , formatCombo
  , toCommandShortcut
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, elem)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith, split, toUpper)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | Every action a keyboard shortcut can trigger in the sidebar.
data Cmd
  = NewGroup
  | FocusSearch
  | ZoomIn
  | ZoomOut
  | ResetZoom
  | Export
  | Import

derive instance eqCmd :: Eq Cmd

-- | Show as the stable storage key — handy in test output.
instance showCmd :: Show Cmd where
  show = keyOf

-- | Stable order; also the order shown on the options page.
allCmds :: Array Cmd
allCmds = [ NewGroup, FocusSearch, ZoomIn, ZoomOut, ResetZoom, Export, Import ]

-- | Stable key under which an override for this command is persisted. Never
-- | localized — changing these orphans existing user overrides.
keyOf :: Cmd -> String
keyOf = case _ of
  NewGroup -> "newGroup"
  FocusSearch -> "focusSearch"
  ZoomIn -> "zoomIn"
  ZoomOut -> "zoomOut"
  ResetZoom -> "resetZoom"
  Export -> "export"
  Import -> "import"

-- | Human label shown on the options page.
labelOf :: Cmd -> String
labelOf = case _ of
  NewGroup -> "New group"
  FocusSearch -> "Focus search"
  ZoomIn -> "Zoom in"
  ZoomOut -> "Zoom out"
  ResetZoom -> "Reset zoom"
  Export -> "Export"
  Import -> "Import"

-- | Default combo when the user hasn't set an override. Single, unmodified keys
-- | that don't collide with Firefox's own sidebar shortcuts; all fully
-- | re-bindable from the options page.
defaultBinding :: Cmd -> String
defaultBinding = case _ of
  NewGroup -> "n"
  FocusSearch -> "/"
  ZoomIn -> "="
  ZoomOut -> "-"
  ResetZoom -> "0"
  Export -> "e"
  Import -> "i"

-- | Effective combo for a command given the stored overrides; a missing or empty
-- | override falls back to the default.
bindingFor :: Object String -> Cmd -> String
bindingFor overrides c = case Object.lookup (keyOf c) overrides of
  Just s | s /= "" -> s
  _ -> defaultBinding c

-- | Which command (if any) a pressed combo triggers, honoring overrides.
cmdForCombo :: Object String -> String -> Maybe Cmd
cmdForCombo overrides combo = Array.find (\c -> bindingFor overrides c == combo) allCmds

-- | Pretty-print a canonical combo for display: upper-case a trailing single
-- | character ("shift+n" -> "Shift+N", "n" -> "N"), leave everything else as-is.
formatCombo :: String -> String
formatCombo combo = case Array.unsnoc (split (Pattern "+") combo) of
  Just { init, last } | SCU.length last == 1 -> joinWith "+" (init <> [ toUpper last ])
  _ -> combo

-- | Translate a captured canonical combo (e.g. "Ctrl+Shift+y") into a
-- | WebExtensions `commands` shortcut (e.g. "Ctrl+Shift+Y"), or explain why it
-- | can't be one. Unlike the in-page shortcuts, a browser command needs a
-- | primary modifier (Ctrl/Alt/Command/MacCtrl) and a restricted key set. `mac`
-- | selects the Mac modifier names (the Control key -> MacCtrl, the ⌘ key ->
-- | Command). The browser still validates the result, so this is a best-effort
-- | shaping that rejects the obviously-impossible up front.
toCommandShortcut :: Boolean -> String -> Either String String
toCommandShortcut mac combo = case Array.unsnoc (split (Pattern "+") combo) of
  Nothing -> Left "No key was pressed."
  Just { init: rawMods, last: rawKey } -> case toCommandKey rawKey of
    Nothing -> Left "That key can't be used for a browser shortcut."
    Just key ->
      let mods = Array.nub (map (toCommandMod mac) rawMods)
      in if Array.length mods > 2 then
           Left "Use at most two modifiers (for example Ctrl and Shift)."
         -- function keys may stand alone; every other key needs a primary modifier
         else if isFunctionKey key || any isPrimaryMod mods then
           Right (joinWith "+" (orderMods mods <> [ key ]))
         else
           Left "Add a modifier such as Ctrl or Alt — browser shortcuts require one."

-- valid `commands` keys: A-Z, 0-9, F1-F12, a few named keys; nothing else.
toCommandKey :: String -> Maybe String
toCommandKey k
  | SCU.length k == 1 =
      let u = toUpper k
      in if isAsciiAlphaNum u then Just u
         else case k of
           "," -> Just "Comma"
           "." -> Just "Period"
           _ -> Nothing
  | otherwise = case k of
      "Space" -> Just "Space"
      "ArrowUp" -> Just "Up"
      "ArrowDown" -> Just "Down"
      "ArrowLeft" -> Just "Left"
      "ArrowRight" -> Just "Right"
      "Home" -> Just "Home"
      "End" -> Just "End"
      "PageUp" -> Just "PageUp"
      "PageDown" -> Just "PageDown"
      "Insert" -> Just "Insert"
      "Delete" -> Just "Delete"
      _ | k `elem` fKeys -> Just k
        | otherwise -> Nothing

-- F1-F19: Firefox accepts up to F19 for command shortcuts (135+; we target 142).
fKeys :: Array String
fKeys = map (\n -> "F" <> show n) (Array.range 1 19)

isFunctionKey :: String -> Boolean
isFunctionKey k = k `elem` fKeys

isAsciiAlphaNum :: String -> Boolean
isAsciiAlphaNum s = (s >= "A" && s <= "Z") || (s >= "0" && s <= "9")

toCommandMod :: Boolean -> String -> String
toCommandMod mac = case _ of
  "Meta" -> "Command"
  "Ctrl" -> if mac then "MacCtrl" else "Ctrl"
  m -> m

isPrimaryMod :: String -> Boolean
isPrimaryMod m = m == "Ctrl" || m == "Alt" || m == "Command" || m == "MacCtrl"

-- the primary modifier(s) first, Shift last (Firefox's canonical order)
orderMods :: Array String -> Array String
orderMods ms = Array.filter (_ /= "Shift") ms <> Array.filter (_ == "Shift") ms
