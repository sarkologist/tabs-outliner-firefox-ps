-- | The options page: a small Halogen view for configuring the sidebar's
-- | keyboard shortcuts. Each row shows an action and its current combo; "Change"
-- | records the next keypress (Effect.Settings.captureCombo) and persists it,
-- | "Reset" drops the override back to the default. Bindings live in the shared
-- | localStorage, so the sidebar picks up changes on its next keypress with no
-- | reload.
module Options.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
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
  }

data Action
  = Initialize
  | StartRecord Sh.Cmd
  | Captured String
  | ResetOne Sh.Cmd
  | ResetAll

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { overrides: Object.empty, recording: Nothing, listener: Nothing }
  , render
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    overrides <- H.liftEffect Settings.getShortcuts
    H.modify_ _ { overrides = overrides, listener = Just listener }

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
    , HH.h2_ [ HH.text "Toggle the sidebar" ]
    , HH.p [ HP.class_ (ClassName "hint") ]
        [ HH.text "Opening and closing the sidebar is a browser-level shortcut (default Ctrl+Shift+Y, or Cmd+Shift+Y on macOS; unset on Linux, where that combo is Firefox's Downloads shortcut). It has to work even when the sidebar is closed, so Firefox handles it directly — set or change it in about:addons → gear menu → Manage Extension Shortcuts." ]
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
