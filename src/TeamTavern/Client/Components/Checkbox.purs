module TeamTavern.Client.Components.Checkbox where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

checkbox :: forall slots action. Boolean -> HH.HTML slots action
checkbox checked =
    HH.div
    [ HS.class_ if checked then "checked-checkbox" else "checkbox" ]
    [ HH.i [ HS.class_ "fas fa-check" ] [] ]

checkboxIconLabel :: forall slots action. String -> HH.HTML slots action
checkboxIconLabel label = HH.span [ HS.class_ "checkbox-icon-label" ] [ HH.text label ]

checkboxLabel :: forall slots action. String -> HH.HTML slots action
checkboxLabel label = HH.span [ HS.class_ "checkbox-label" ] [ HH.text label ]

checkboxIconInput :: forall slots action.
    HH.HTML slots action -> String -> Boolean -> action -> HH.HTML slots action
checkboxIconInput icon label checked onChecked =
    HH.div
    [ HS.class_ "checkbox-input"
    , HE.onClick $ const $ Just onChecked
    ]
    [ icon
    , checkboxIconLabel label
    , checkbox checked
    ]

checkboxInput :: forall slots action.
    Boolean -> (Boolean -> action) -> String -> HH.HTML slots action
checkboxInput checked onChecked label =
    HH.div
    [ HS.class_ "checkbox-input"
    , HE.onClick $ const $ Just $ onChecked $ not checked
    ]
    [ checkbox checked
    , checkboxLabel label
    ]

data CheckboxState = Checked | Unchecked | Indeterminate

checkbox' :: forall slots action. CheckboxState -> HH.HTML slots action
checkbox' state =
    HH.div
    [ HS.class_
        case state of
        Checked -> "checked-checkbox"
        Indeterminate -> "checked-checkbox"
        Unchecked -> "checkbox"
    ]
    [ HH.i
        [ HS.class_
            case state of
            Checked -> "fas fa-check"
            Indeterminate -> "fas fa-minus"
            Unchecked -> "fas fa-check"
        ]
        []
    ]
