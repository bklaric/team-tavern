module TeamTavern.Client.Components.RadioCard where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Components.Picture (picture)
import TeamTavern.Client.Snippets.Class as HS

radioCards :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
radioCards = HH.div [ HS.class_ "radio-cards" ]

radioCard :: forall slots action.
    String -> Boolean -> action -> Array (HH.HTML slots action) -> HH.HTML slots action
radioCard backgroundUrl selected onSelect children =
    HH.div
    [ HS.class_ if selected then "selected-radio-card" else "radio-card"
    , HE.onClick $ const $ Just onSelect
    ]
    [ picture "radio-card-image" "Game banner" backgroundUrl
    , HH.div
        [ HS.class_ if selected then "selected-radio-card-check" else "radio-card-check" ]
        [ HH.i [ HS.class_ $ "fas fa-check" ] [] ]
    , HH.div
        [ HS.class_ if selected then "selected-radio-card-ribbon" else "radio-card-ribbon" ]
        [ HH.span [ HS.class_ "radio-card-ribbon-text" ] children ]
    ]
