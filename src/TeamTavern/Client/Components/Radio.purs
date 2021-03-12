module TeamTavern.Client.Components.Radio where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

radio :: forall slots action. Boolean -> HH.HTML slots action
radio checked =
    HH.div
    [ HS.class_ if checked then "checked-radio" else "radio" ]
    [ HH.i [ HS.class_ "fas fa-check" ] [] ]

radioIconLabel :: forall slots action. String -> HH.HTML slots action
radioIconLabel label = HH.span [ HS.class_ "radio-icon-label" ] [ HH.text label ]

radioLabel :: forall slots action. String -> HH.HTML slots action
radioLabel label = HH.span [ HS.class_ "radio-label" ] [ HH.text label ]

radioIconInput :: forall slots action.
    HH.HTML slots action -> String -> Boolean -> action -> HH.HTML slots action
radioIconInput icon label checked onChecked =
    HH.div
    [ HS.class_ "radio-input"
    , HE.onClick $ const $ Just onChecked
    ]
    [ icon
    , radioIconLabel label
    , radio checked
    ]

radioInput :: forall slots action.
    Boolean -> (Boolean -> action) -> String -> HH.HTML slots action
radioInput checked onChecked label =
    HH.div
    [ HS.class_ "radio-input"
    , HE.onClick $ const $ Just $ onChecked $ not checked
    ]
    [ radio checked
    , radioLabel label
    ]
