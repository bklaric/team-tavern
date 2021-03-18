module TeamTavern.Client.Components.Checkable where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

checkables :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
checkables = HH.div [ HS.class_ "checkables" ]

checkbox :: forall slots action. Boolean -> HH.HTML slots action
checkbox checked =
    HH.div
    [ HS.class_ if checked then "checked-checkbox" else "checkbox" ]
    [ HH.i [ HS.class_ "fas fa-check" ] [] ]

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

radio :: forall slots action. Boolean -> HH.HTML slots action
radio checked =
    HH.div
    [ HS.class_ if checked then "checked-radio" else "radio" ]
    [ HH.i [ HS.class_ "fas fa-check" ] [] ]
