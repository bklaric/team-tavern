module TeamTavern.Client.Components.Confirm (confirm) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Snippets.Class as HS

-- A confirmation standing in place of the button that asked for it, saying
-- what will be lost, with the counts.
confirm :: ∀ w i.
    { id :: String
    , title :: String
    , text :: String
    , action :: Array (HH.HTML w i)
    , onConfirm :: i
    , cancel :: String
    , onCancel :: i
    }
    -> HH.HTML w i
confirm { id, title, text, action, onConfirm, cancel, onCancel } =
    HH.div
    [ HS.class_ "confirm"
    , HPA.role "alertdialog"
    , HPA.labelledBy titleId
    , HPA.describedBy textId
    ]
    [ HH.h3 [ HP.id titleId ] [ HH.text title ]
    , HH.p [ HP.id textId ] [ HH.text text ]
    , HH.div [ HS.class_ "confirm-actions" ]
        [ button Destructive Regular onConfirm action
        , button Text Regular onCancel [ HH.text cancel ]
        ]
    ]
    where
    titleId = id <> "-title"
    textId = id <> "-text"
