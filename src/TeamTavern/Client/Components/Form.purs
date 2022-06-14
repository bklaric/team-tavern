module TeamTavern.Client.Components.Form where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Internal.Types (Event)

form :: forall slots action.
    (Event -> action) -> Array (HH.HTML slots action) -> HH.HTML slots action
form onSubmit content = HH.form [ HS.class_ "form", HE.onSubmit onSubmit ] content

submitButton :: forall slots action. String -> String -> String -> Boolean -> HH.HTML slots action
submitButton icon text submittingText submitting =
    HH.button [ HS.class_ "form-submit-button", HP.disabled submitting ]
    [ HH.i [ HS.class_ $ icon <> " button-icon" ] []
    , HH.text if submitting then submittingText else text
    ]

formError :: forall slots action. String -> Boolean -> Array (HH.HTML slots action)
formError text true = [ HH.p [ HS.class_ "form-error" ] [ HH.text text ] ]
formError _ false = []

otherFormError :: forall slots action. Boolean -> Array (HH.HTML slots action)
otherFormError = formError "Something unexpected went wrong! Please try again later."
