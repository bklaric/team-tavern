module TeamTavern.Client.Pages.Player.CreateTeam (Input, Output, Slot, createTeam) where

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
import TeamTavern.Client.Pages.Wizard.EnterTeamDetails (enterTeamDetails)
import TeamTavern.Client.Pages.Wizard.EnterTeamDetails as EnterTeamDetails
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Request (nothingIfEmpty, post)
import TeamTavern.Server.Team.Create (TeamModel)
import TeamTavern.Server.Team.Create as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = { nickname :: String }

type Output = { handle :: String }

type State =
    { nickname :: String
    , details :: EnterTeamDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = UpdateDetails EnterTeamDetails.Output
    | SendRequest Event

type ChildSlots = (enterTeamDetails :: EnterTeamDetails.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    form SendRequest $
    [ enterTeamDetails details (Just <<< UpdateDetails)
    , submitButton "fas fa-user-plus" "Create team" "Creating team..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe (Either Create.BadContent Create.OkContent))
sendRequest state @ { nickname, details } =
    post
    ({ name: details.name
    , website: nothingIfEmpty details.website
    , ageFrom: details.ageFrom
    , ageTo: details.ageTo
    , locations: details.locations
    , languages: details.languages
    , microphone: details.microphone
    , discordServer: nothingIfEmpty details.discordServer
    , timezone: details.timezone
    , weekdayFrom: details.weekdayFrom
    , weekdayTo: details.weekdayTo
    , weekendFrom: details.weekendFrom
    , weekendTo: details.weekendTo
    , about: details.about
    } :: TeamModel)

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
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

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ nickname } ->
        { nickname
        , details: EnterTeamDetails.emptyInput
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

createTeam
    :: forall action children left
    .  Input
    -> (Modal.Output Output -> Maybe action)
    -> HH.ComponentHTML action (createTeam :: Slot | children) (Async left)
createTeam input handleMessage = HH.slot
    (SProxy :: SProxy "createTeam") unit
    (Modal.component "Create a team" component) input handleMessage
