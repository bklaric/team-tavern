module TeamTavern.Client.Components.Checkbox where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

checkbox :: forall action slots.
    HH.HTML slots action -> String -> Boolean -> action -> HH.HTML slots action
checkbox icon text selected onInput =
    HH.div
    [ HS.class_ "checkbox"
    , HE.onClick $ const $ Just onInput
    ]
    [ icon
    , HH.text text
    , HH.div
        [ HS.class_ if selected then "selected-checkbox-check" else "checkbox-check" ]
        [ HH.i [ HS.class_ $ "fas fa-check" ] [] ]
    ]
