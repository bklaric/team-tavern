module TeamTavern.Client.Pages.Player.CreateTeam (Slot, createTeam) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), match)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.TeamFormInput (teamFormInput)
import TeamTavern.Client.Components.Team.TeamFormInput as EnterTeamDetails
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Request (post)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Team.Create (TeamModel)
import TeamTavern.Server.Team.Create as Create
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

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    form SendRequest $
    [ teamFormInput details (Just <<< UpdateDetails)
    , submitButton "fas fa-user-plus" "Create team" "Creating team..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe (Either Create.BadContent Create.OkContent))
sendRequest state @ { details } =
    post "/api/teams"
    ({ name: details.name
    , website: details.website
    , ageFrom: details.ageFrom
    , ageTo: details.ageTo
    , locations: details.locations
    , languages: details.languages
    , microphone: details.microphone
    , discordServer: details.discordServer
    , timezone: details.timezone
    , weekdayFrom: details.weekdayFrom
    , weekdayTo: details.weekdayTo
    , weekendFrom: details.weekendFrom
    , weekendTo: details.weekendTo
    , about: details.about
    } :: TeamModel)

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    timezone <- getClientTimezone
    H.modify_ \state -> state { details = state.details { timezone = Just timezone } }
handleAction (UpdateDetails details) =
    H.modify_ \state -> state
        { details = state.details
            { name = details.name
            , website = details.website
            , ageFrom = details.ageFrom
            , ageTo = details.ageTo
            , locations = details.locations
            , languages = details.languages
            , microphone = details.microphone
            , discordServer = details.discordServer
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right { handle }) -> navigate_ $ "/teams/" <> handle
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { name: const state { details = state.details { nameError = true } }
                , website: const state { details = state.details { websiteError = true } }
                , discordServer: const state { details = state.details { discordServerError = true } }
                , about: const state { details = state.details { aboutError = true } }
                }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , details = currentState.details
                    { nameError = false
                    , websiteError = false
                    , discordServerError = false
                    , aboutError = false
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
                , discordServerError = false
                , aboutError = false
                }
            }

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
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
    :: forall action children left
    .  (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (createTeam :: Slot | children) (Async left)
createTeam handleMessage = HH.slot
    (SProxy :: SProxy "createTeam") unit
    (Modal.component "Create team" component) unit handleMessage
