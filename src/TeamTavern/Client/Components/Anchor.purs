module TeamTavern.Client.Components.Anchor where

import Prelude

import Async (Async)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Snippets.Class as HS

textAnchor :: ∀ slots action. String -> String -> String -> HH.HTML slots action
textAnchor class_ url text =
    HH.a [ HS.class_ class_, HP.target "_blank", HP.rel "noopener", HP.href url ] [ HH.text text ]

textAnchor_ :: ∀ slots action. String -> String -> HH.HTML slots action
textAnchor_ url text =
    HH.a [ HP.target "_blank", HP.rel "noopener", HP.href url ] [ HH.text text ]

trackedTextAnchor :: forall q o l.
    H.Component q {text :: String, url :: String} o (Async l)
trackedTextAnchor = Hooks.component \_ {url, text} -> Hooks.do
    Hooks.pure $
        HH.a
        [ HP.target "_blank", HP.rel "noopener", HP.href url
        , HE.onMouseDown $ const $ track "Link click" {text}
        ]
        [ HH.text text ]

iconAnchor :: ∀ slots action. String -> String -> String -> HH.HTML slots action
iconAnchor url title icon =
    HH.a
    [ HP.href url, HP.target "_blank", HP.rel "noopener", HP.title title ]
    [ HH.i [ HS.class_ icon ] [] ]

mailtoAnchor :: ∀ slots action. String -> String -> HH.HTML slots action
mailtoAnchor email text = HH.a [ HP.href $ "mailto:" <> email ] [ HH.text text ]
