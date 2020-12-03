module TeamTavern.Client.Components.RadioButton where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

radioButtons :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
radioButtons = HH.div [ HS.class_ "radio-buttons" ]

radioButton :: forall slots action.
    Boolean -> action -> Array (HH.HTML slots action) -> HH.HTML slots action
radioButton selected onSelect children =
    HH.button
    [ HS.class_ if selected then "selected-radio-button" else "radio-button"
    , HE.onClick $ const $ Just onSelect
    ] $
    children
    <>
    [ HH.div
        [ HS.class_ if selected then "selected-radio-button-check" else "radio-button-check" ]
        [ HH.i [ HS.class_ $ "fas fa-check" ] [] ]
    ]

radioButtonDescription :: forall slots action. String -> HH.HTML slots action
radioButtonDescription text = HH.p [ HS.class_ "radio-button-description" ] [ HH.text text]
