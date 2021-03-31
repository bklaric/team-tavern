module TeamTavern.Client.Pages.Profiles.CreateAlert where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.MultiMap (toUnfoldable')
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), match)
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Boarding.Boarding (boardingDescription)
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel, requiredTextLineInput)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.TeamFormInput (teamFormInput)
import TeamTavern.Client.Components.Team.TeamFormInput as EnterTeamDetails
import TeamTavern.Client.Pages.Profile.Filters (Filters)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Request (post, postNoContent, postNoContent')
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Routes.CreateAlert (PlayerOrTeam)
import TeamTavern.Routes.CreateAlert as CreateAlert
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamModel)
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

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall slots left. State -> H.ComponentHTML Action slots (Async left)
render { email, emailError, otherError, submitting } =
    form SendRequest $
    [ boardingDescription """Create a profile alert using the specified filters
        and we will send you an email when a matching profile has been created."""
    , inputGroup
        [ inputLabel "fas fa-envelope" "Email address"
        , requiredTextLineInput email UpdateEmail
        ]
    ]
    <> inputError emailError "This doesn't look like a valid email address."
    <> [ submitButton "fas fa-bell" "Create profile alert" "Creating profile alert..." submitting ]
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
                # Record.insert (SProxy :: _ "fields")
                    (filters.fieldValues # toUnfoldable' <#> \(Tuple fieldKey optionKeys) ->
                        { fieldKey, optionKeys })
                # pick

            }
    postNoContent "/api/teams" body

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction (UpdateEmail email) =
    H.modify_ _ { email = email }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    state <- H.modify _ { emailError = false, otherError = false, submitting = true }
    response <- H.lift $ sendRequest state
    case response of
        Just (Right _) -> pure unit -- Raise output to close modal.
        Just (Left _) -> H.modify_ _ { emailError = true, submitting = false }
        Nothing -> H.modify_ _ { otherError = true, submitting = false }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
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

createAlert input handleMessage = HH.slot
    (SProxy :: _ "createAlert") unit
    (Modal.component "Create a profile alert" component) input handleMessage
