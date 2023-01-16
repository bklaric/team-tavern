module TeamTavern.Client.Pages.Player.CreateTeam (createTeam) where

import Prelude

import Async (Async)
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
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Request (post)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Routes.Team.CreateTeam as CreateTeam
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type State =
    { details :: EnterTeamDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = Initialize
    | UpdateDetails EnterTeamDetails.Output
    | SendRequest Event

type ChildSlots = (teamFormInput :: EnterTeamDetails.Slot)

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    form SendRequest $
    [ teamFormInput details UpdateDetails
    , submitButton "fas fa-user-plus" "Create team" "Creating team..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: ∀ left. State -> Async left (Maybe (Either CreateTeam.BadContent CreateTeam.OkContent))
sendRequest { details } = post "/api/teams" (pick details :: CreateTeam.RequestContent)

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    timezone <- getClientTimezone
    H.modify_ \state -> state { details = state.details { timezone = Just timezone } }
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
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right { handle }) -> do
            track_ "Team create"
            navigate_ $ "/teams/" <> handle
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { name: const state { details = state.details { nameError = true } }
                , website: const state { details = state.details { websiteError = true } }
                }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , details = currentState.details
                    { nameError = false
                    , websiteError = false
                    }
                }
            )
            badContent
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            , details = currentState.details
                { nameError = false
                , websiteError = false
                }
            }

component :: ∀ query input output left.
    H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const
        { details: EnterTeamDetails.emptyInput
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

createTeam
    :: ∀ action children left
    .  (Modal.Output Void -> action)
    -> HH.ComponentHTML action (createTeam :: Modal.Slot_ | children) (Async left)
createTeam handleMessage = HH.slot
    (Proxy :: _ "createTeam") unit
    (Modal.component "Create team" component) unit handleMessage
