module TeamTavern.Client.Components.Divider where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

divider :: forall slots action. HH.HTML slots action
divider = HH.span [ HP.class_ $ H.ClassName "divider" ] [ HH.text "•"]

whiteDivider :: forall slots action. HH.HTML slots action
whiteDivider = HH.span [ HP.class_ $ H.ClassName "white-divider" ] [ HH.text "•"]
