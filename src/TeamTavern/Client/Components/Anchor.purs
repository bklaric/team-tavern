module TeamTavern.Client.Components.Anchor where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

textAnchor :: forall slots action. String -> String -> String -> HH.HTML slots action
textAnchor class_ url text =
    HH.a [ HS.class_ class_, HP.target "_blank", HP.rel "noopener", HP.href url ] [ HH.text text ]

textAnchor_ :: forall slots action. String -> String -> HH.HTML slots action
textAnchor_ url text =
    HH.a [ HP.target "_blank", HP.rel "noopener", HP.href url ] [ HH.text text ]

iconAnchor :: forall slots action. String -> String -> String -> HH.HTML slots action
iconAnchor url title icon =
    HH.a
    [ HP.href url, HP.target "_blank", HP.rel "noopener", HP.title title ]
    [ HH.i [ HS.class_ icon ] [] ]

mailtoAnchor :: forall slots action. String -> String -> HH.HTML slots action
mailtoAnchor email text = HH.a [ HP.href $ "mailto:" <> email ] [ HH.text text ]
