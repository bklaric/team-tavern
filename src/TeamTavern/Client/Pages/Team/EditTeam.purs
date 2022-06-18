module TeamTavern.Client.Pages.Team.EditTeam where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.TeamFormInput (teamFormInput)
import TeamTavern.Client.Components.Team.TeamFormInput as EnterTeamDetails
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Team.CreateTeam as CreateTeam
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input fields =
    { handle :: String
    , organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , weekendOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    | fields
    }

type State =
    { handle :: String
    , details :: EnterTeamDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = Initialize
    | UpdateDetails EnterTeamDetails.Output
    | SendRequest Event

type ChildSlots = (teamFormInput :: EnterTeamDetails.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    form SendRequest $
    [ teamFormInput details UpdateDetails
    , submitButton "fas fa-edit" "Edit team" "Editting team..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe (Either CreateTeam.BadContent Unit))
sendRequest { handle, details } =
    putNoContent ("/api/teams/" <> handle) (pick details :: CreateTeam.RequestContent)

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state.details.timezone of
        Just _ -> pure unit
        Nothing -> do
            timezone <- getClientTimezone
            H.put state { details = state.details { timezone = Just timezone } }
handleAction (UpdateDetails details) =
    H.modify_ \state -> state
        { details = state.details
            { organization = details.organization
            , ageFrom = details.ageFrom
            , ageTo = details.ageTo
            , locations = details.locations
            , languages = details.languages
            , microphone = details.microphone
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            }
        }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _
        { submitting = true
        , otherError = false
        , details
            { nameError = false
            , websiteError = false
            }
        }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right _) -> hardNavigate $ "/teams/" <> currentState.handle
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { name: const state { details { nameError = true } }
                , website: const state { details { websiteError = true } }
                }
                error
            )
            (currentState { submitting = false })
            badContent
        Nothing -> H.put currentState { submitting = false, otherError = true }

component :: forall query output fields left.
    H.Component query (Input fields) output (Async left)
component = H.mkComponent
    { initialState: \team ->
        { handle: team.handle
        , details: EnterTeamDetails.emptyInput
            { organization = team.organization
            , ageFrom = team.ageFrom
            , ageTo = team.ageTo
            , locations = team.locations
            , languages = team.languages
            , microphone = team.microphone
            , timezone = team.timezone
            , weekdayFrom = team.weekdayOnline <#> _.sourceFrom
            , weekdayTo = team.weekdayOnline <#> _.sourceTo
            , weekendFrom = team.weekendOnline <#> _.sourceFrom
            , weekendTo = team.weekendOnline <#> _.sourceTo
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

editTeam
    :: forall fields action children left
    .  (Input fields)
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (editTeam :: Slot | children) (Async left)
editTeam input handleMessage = HH.slot
    (Proxy :: _ "editTeam") unit
    (Modal.component "Edit team" component) input handleMessage
