module TeamTavern.Client.Components.RadioCard where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

radioCards :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
radioCards = HH.div [ HS.class_ "radio-cards" ]

radioCard :: ∀ slots action.
    HH.HTML slots action -> Boolean -> action -> Array (HH.HTML slots action) -> HH.HTML slots action
radioCard image selected onSelect children =
    HH.div
    [ HS.class_ if selected then "selected-radio-card" else "radio-card"
    , HE.onClick $ const onSelect
    ]
    [ HH.div [ HS.class_ "radio-card-image" ] [ image ]
    , HH.div
        [ HS.class_ if selected then "selected-radio-card-check" else "radio-card-check" ]
        [ HH.i [ HS.class_ $ "fas fa-check" ] [] ]
    , HH.div
        [ HS.class_ if selected then "selected-radio-card-ribbon" else "radio-card-ribbon" ]
        [ HH.span [ HS.class_ "radio-card-ribbon-text" ] children ]
    ]
