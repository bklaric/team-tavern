module TeamTavern.Client.Pages.Profiles.CreateAlert where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MultiMap (toUnfoldable')
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Boarding.Boarding (boardingDescription)
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel, requiredTextLineInput)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Profile.Filters (Filters)
import TeamTavern.Client.Script.Request (postNoContent)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Routes.Alert.CreateAlert (PlayerOrTeam(..))
import TeamTavern.Routes.Alert.CreateAlert as CreateAlert
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { handle :: String
    , playerOrTeam :: PlayerOrTeam
    , filters :: Filters
    }

type State =
    { handle :: String
    , playerOrTeam :: PlayerOrTeam
    , email :: String
    , emailError :: Boolean
    , filters :: Filters
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = UpdateEmail String
    | SendRequest Event

data Output = AlertCreated

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

render :: forall slots left. State -> H.ComponentHTML Action slots (Async left)
render { email, emailError, otherError, submitting } =
    form SendRequest $
    [ boardingDescription """Create a profile alert using the specified filters
        and we will send you an email when a matching profile has been created."""
    , inputGroup $
        [ inputLabel "fas fa-envelope" "Email address"
        , requiredTextLineInput email UpdateEmail
        ]
        <> inputError emailError "This doesn't look like a valid email address."
    ]
    <> [ submitButton "fas fa-bell" "Create alert" "Creating alert..." submitting ]
    <> otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe (Either CreateAlert.BadContent Unit))
sendRequest { handle, playerOrTeam, email, filters } = do
    timezone <- getClientTimezone
    let (body :: CreateAlert.RequestContent) =
            { handle
            , playerOrTeam
            , email
            , timezone
            , filters:
                filters
                # Record.insert (Proxy :: _ "fields")
                    (filters.fieldValues # toUnfoldable' <#> \(Tuple fieldKey optionKeys) ->
                        { fieldKey, optionKeys })
                # pick
            }
    postNoContent "/api/alerts" body

handleAction :: forall slots left. Action -> H.HalogenM State Action slots Output (Async left) Unit
handleAction (UpdateEmail email) = H.modify_ _ { email = email }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    state <- H.modify _ { emailError = false, otherError = false, submitting = true }
    response <- H.lift $ sendRequest state
    case response of
        Just (Right _) -> H.raise AlertCreated
        Just (Left _) -> H.modify_ _ { emailError = true, submitting = false }
        Nothing -> H.modify_ _ { otherError = true, submitting = false }

component :: forall query left. H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ handle, playerOrTeam, filters } ->
        { handle
        , playerOrTeam
        , filters
        , email: ""
        , emailError: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

title :: PlayerOrTeam -> String
title Player = "Create player profile alert"
title Team = "Create team profile alert"

createAlert
    :: forall slots action left
    .  Input
    -> (Modal.Output Output -> action)
    -> HH.ComponentHTML action (createAlert :: Slot | slots) (Async left)
createAlert input handleMessage = HH.slot
    (Proxy :: _ "createAlert") unit
    (Modal.component (title input.playerOrTeam) component) input handleMessage
