module TeamTavern.Client.Components.Flow
    ( TextField
    , flow
    , flowError
    , flowLead
    , flowLink
    , formTight
    , submitButton
    , textField
    ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Field (field, field_)
import TeamTavern.Client.Components.Input (input)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Event (Event)

-- A page of one narrow column, such as signing in: a heading, a line under it
-- and a form.
flow :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
flow = HH.div [ HS.class_ "flow flow-narrow" ]

flowLead :: ∀ w i. String -> HH.HTML w i
flowLead text = HH.p [ HS.class_ "flow-lead" ] [ HH.text text ]

-- A link to another page of the site, inside a sentence.
flowLink :: ∀ w m. MonadEffect m => String -> String -> HH.HTML w (m Unit)
flowLink path text = HH.a [ HP.href path, HE.onClick $ navigateWithEvent_ path ] [ HH.text text ]

-- The form's fields are validated by the page, which says what is wrong under
-- each, so the browser's own checks are off.
formTight :: ∀ w i. (Event -> i) -> Array (HH.HTML w i) -> HH.HTML w i
formTight onSubmit = HH.form [ HS.class_ "form form-tight", HP.noValidate true, HE.onSubmit onSubmit ]

type TextField i =
    { id :: String
    , label :: String
    , type_ :: HP.InputType
    , autocomplete :: HP.AutocompleteType
    , hint :: Maybe String
    , error :: Maybe String
    , value :: String
    , onInput :: String -> i
    }

textField :: ∀ w i. TextField i -> HH.HTML w i
textField { id, label, type_, autocomplete, hint, error, value, onInput } =
    field ((field_ id label) { hint = hint, error = error })
    [ input [ HP.id id, HP.type_ type_, HP.autocomplete autocomplete ]
        { value, placeholder: "", onInput }
    ]

-- What went wrong with the form as a whole rather than with one field.
flowError :: ∀ w i. String -> HH.HTML w i
flowError text = HH.p [ HS.class_ "field-error", HPA.role "alert" ] [ Icons.circleAlert, HH.text text ]

-- The form's one filled button, which does nothing more while the form is
-- being sent.
submitButton :: ∀ w i. Boolean -> String -> HH.HTML w i
submitButton sending label =
    HH.button [ HS.class_ "button button-primary", HP.type_ HP.ButtonSubmit, HP.disabled sending ]
    [ HH.text label ]
