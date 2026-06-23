module Sidebar.Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI component unit body

-- A static placeholder component. Replaced by the real tree view in M3.
component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render: \_ ->
        HH.div
          [ HP.id "app" ]
          [ HH.h1_ [ HH.text "Tabs Outliner" ]
          , HH.p_ [ HH.text "hello" ]
          ]
    , eval: H.mkEval H.defaultEval
    }
