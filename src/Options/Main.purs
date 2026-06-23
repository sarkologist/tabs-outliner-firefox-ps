-- | The options page: configure the sidebar's keyboard shortcuts. The in-page
-- | toolbar shortcuts live in shared localStorage (Effect.Settings) and are
-- | edited in the table. The sidebar-toggle shortcut is a browser-level command
-- | (Effect.Commands, the WebExtensions commands API) — it must work when the
-- | sidebar is closed, so the browser owns it and we edit it via commands.update.
-- | If that API is unavailable, the toggle section falls back to a "configure it
-- | in Firefox" note.
module Options.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Commands as Commands
import Effect.Settings as Settings
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Model.Shortcuts as Sh

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI component unit body

type State =
  { overrides :: Object String
  , recording :: Maybe Sh.Cmd
  , listener :: Maybe (HS.Listener Action)
  , toggle :: Maybe String -- Nothing = commands API unavailable; Just s = current ("" if unset)
  , toggleMac :: Boolean
  , toggleRecording :: Boolean
  , toggleError :: Maybe String
  }

data Action
  = Initialize
  | StartRecord Sh.Cmd
  | Captured String
  | ResetOne Sh.Cmd
  | ResetAll
  | StartRecordToggle
  | CapturedToggle String
  | ResetToggle

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { overrides: Object.empty
      , recording: Nothing
      , listener: Nothing
      , toggle: Nothing
      , toggleMac: false
      , toggleRecording: false
      , toggleError: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    overrides <- H.liftEffect Settings.getShortcuts
    mac <- H.liftEffect Commands.isMac
    toggle <- H.liftAff Commands.getSidebarToggle
    H.modify_ _ { overrides = overrides, listener = Just listener, toggle = toggle, toggleMac = mac }

  StartRecord cmd -> do
    st <- H.get
    H.modify_ _ { recording = Just cmd }
    case st.listener of
      Just l -> H.liftEffect $ Settings.captureCombo \combo -> HS.notify l (Captured combo)
      Nothing -> pure unit

  -- Escape cancels recording without binding; any other combo is saved.
  Captured combo -> do
    st <- H.get
    case st.recording of
      Just cmd | combo /= "Escape" -> do
        let next = Object.insert (Sh.keyOf cmd) combo st.overrides
        H.liftEffect (Settings.setShortcuts next)
        H.modify_ _ { overrides = next, recording = Nothing }
      _ -> H.modify_ _ { recording = Nothing }

  ResetOne cmd -> do
    st <- H.get
    let next = Object.delete (Sh.keyOf cmd) st.overrides
    H.liftEffect (Settings.setShortcuts next)
    H.modify_ _ { overrides = next }

  ResetAll -> do
    H.liftEffect (Settings.setShortcuts Object.empty)
    H.modify_ _ { overrides = Object.empty }

  StartRecordToggle -> do
    st <- H.get
    H.modify_ _ { toggleRecording = true, toggleError = Nothing }
    case st.listener of
      Just l -> H.liftEffect $ Settings.captureCombo \combo -> HS.notify l (CapturedToggle combo)
      Nothing -> pure unit

  CapturedToggle combo
    | combo == "Escape" || combo == "" -> H.modify_ _ { toggleRecording = false }
    | otherwise -> do
        st <- H.get
        case Sh.toCommandShortcut st.toggleMac combo of
          Left msg -> H.modify_ _ { toggleRecording = false, toggleError = Just msg }
          Right shortcut -> do
            res <- H.liftAff (Commands.setSidebarToggle shortcut)
            case res of
              Just err -> H.modify_ _ { toggleRecording = false, toggleError = Just err }
              Nothing -> do
                cur <- H.liftAff Commands.getSidebarToggle
                H.modify_ _ { toggle = cur, toggleRecording = false, toggleError = Nothing }

  ResetToggle -> do
    _ <- H.liftAff Commands.resetSidebarToggle
    cur <- H.liftAff Commands.getSidebarToggle
    H.modify_ _ { toggle = cur, toggleRecording = false, toggleError = Nothing }

render :: State -> H.ComponentHTML Action () Aff
render st =
  HH.div [ HP.id "wrap" ]
    [ HH.h1_ [ HH.text "Tabs Outliner — Keyboard shortcuts" ]
    , HH.p [ HP.class_ (ClassName "hint") ]
        [ HH.text "Shortcuts fire in the sidebar while you're not typing in a text box. Click Change, then press the keys you want (modifiers allowed). Press Esc to cancel." ]
    , conflictWarning st.overrides
    , HH.table [ HP.id "shortcuts" ]
        ( [ HH.tr_ [ HH.th_ [ HH.text "Action" ], HH.th_ [ HH.text "Shortcut" ], HH.th_ [] ] ]
            <> map (row st) Sh.allCmds
        )
    , HH.button [ HP.id "reset-all", HE.onClick \_ -> ResetAll ] [ HH.text "Reset all to defaults" ]
    , toggleSection st
    ]

row :: State -> Sh.Cmd -> H.ComponentHTML Action () Aff
row st cmd =
  HH.tr_
    [ HH.td_ [ HH.text (Sh.labelOf cmd) ]
    , HH.td [ HP.class_ (ClassName "combo") ] [ comboCell ]
    , HH.td_
        [ HH.button [ HE.onClick \_ -> StartRecord cmd ] [ HH.text "Change" ]
        , HH.button [ HE.onClick \_ -> ResetOne cmd ] [ HH.text "Reset" ]
        ]
    ]
  where
  comboCell =
    if st.recording == Just cmd then
      HH.span [ HP.class_ (ClassName "recording") ] [ HH.text "Press keys… (Esc to cancel)" ]
    else
      HH.span [ HP.class_ (ClassName "kbd") ] [ HH.text (Sh.formatCombo (Sh.bindingFor st.overrides cmd)) ]

-- | The browser-level sidebar-toggle command. Editable here when the commands
-- | API is present; otherwise a note points at Firefox's own shortcut manager.
toggleSection :: State -> H.ComponentHTML Action () Aff
toggleSection st =
  HH.div_
    ( [ HH.h2_ [ HH.text "Toggle the sidebar" ]
      , HH.p [ HP.class_ (ClassName "hint") ]
          [ HH.text "Opening and closing the sidebar is a browser-level shortcut — it must work even when the sidebar is closed, so the browser handles it. Use a modifier such as Ctrl or Alt (a function key works on its own)." ]
      ] <> bodyHtml
    )
  where
  bodyHtml = case st.toggle of
    Nothing ->
      [ HH.p [ HP.class_ (ClassName "hint") ]
          [ HH.text "Set it in about:addons → gear menu → Manage Extension Shortcuts (default Ctrl+Shift+Y; Cmd+Shift+Y on macOS; unset on Linux)." ]
      ]
    Just current ->
      [ HH.table [ HP.id "toggle" ]
          [ HH.tr_
              [ HH.td_ [ HH.text "Toggle sidebar" ]
              , HH.td [ HP.class_ (ClassName "combo") ] [ toggleCell current ]
              , HH.td_
                  [ HH.button [ HP.id "toggle-change", HE.onClick \_ -> StartRecordToggle ] [ HH.text "Change" ]
                  , HH.button [ HP.id "toggle-reset", HE.onClick \_ -> ResetToggle ] [ HH.text "Reset" ]
                  ]
              ]
          ]
      ] <> errorNote
  toggleCell current =
    if st.toggleRecording then
      HH.span [ HP.class_ (ClassName "recording") ] [ HH.text "Press keys… (Esc to cancel)" ]
    else if current == "" then
      HH.span [ HP.class_ (ClassName "muted") ] [ HH.text "Not set" ]
    else
      HH.span [ HP.class_ (ClassName "kbd") ] [ HH.text current ]
  errorNote = case st.toggleError of
    Just msg -> [ HH.div [ HP.class_ (ClassName "warn"), HP.id "toggle-error" ] [ HH.text msg ] ]
    Nothing -> []

-- | Warn when two actions resolve to the same combo (the first in allCmds order
-- | wins on a real keypress, so the other would be dead).
conflictWarning :: Object String -> H.ComponentHTML Action () Aff
conflictWarning overrides = case dups of
  [] -> HH.text ""
  cs -> HH.div [ HP.class_ (ClassName "warn") ]
    [ HH.text ("The same key is bound to more than one action: " <> joinWith ", " (map Sh.formatCombo cs)) ]
  where
  combos = map (Sh.bindingFor overrides) Sh.allCmds
  dups = Array.nub (Array.filter (\c -> Array.length (Array.filter (eq c) combos) > 1) combos)
