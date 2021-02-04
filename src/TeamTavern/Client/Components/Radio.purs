module TeamTavern.Client.Components.Radio where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

radio :: forall action slots.
    HH.HTML slots action -> String -> Boolean -> action -> HH.HTML slots action
radio icon text selected onInput =
    HH.div
    [ HS.class_ "radio"
    , HE.onClick $ const if selected then Nothing else Just onInput
    ]
    [ icon
    , HH.text text
    , HH.div
        [ HS.class_ if selected then "selected-radio-check" else "radio-check" ]
        [ HH.i [ HS.class_ $ "fas fa-check" ] [] ]
    ]
