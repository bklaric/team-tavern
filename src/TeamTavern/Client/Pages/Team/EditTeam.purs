module TeamTavern.Client.Pages.Team.EditTeam where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), match)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.TeamFormInput (teamFormInput)
import TeamTavern.Client.Components.Team.TeamFormInput as EnterTeamDetails
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Team.Create as Create
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamModel)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input fields =
    { handle :: String
    , name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: Maybe String
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
    , about :: Array String
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
    [ teamFormInput details (Just <<< UpdateDetails)
    , submitButton "fas fa-edit" "Edit team" "Editting team..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe (Either Create.BadContent Unit))
sendRequest state @ { handle, details } = do
    putNoContent
        ("/api/teams/" <> handle)
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
    state <- H.get
    case state.details.timezone of
        Just timezone -> pure unit
        Nothing -> do
            timezone <- getClientTimezone
            H.put state { details = state.details { timezone = Just timezone } }
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
        Just (Right _) -> hardNavigate $ "/teams/" <> currentState.handle
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

component :: forall query output fields left.
    H.Component HH.HTML query (Input fields) output (Async left)
component = H.mkComponent
    { initialState: \team ->
        { handle: team.handle
        , details: EnterTeamDetails.emptyInput
            { name = team.name
            , website = team.website
            , ageFrom = team.ageFrom
            , ageTo = team.ageTo
            , locations = team.locations
            , languages = team.languages
            , microphone = team.microphone
            , discordServer = team.discordServer
            , timezone = team.timezone
            , weekdayFrom = team.weekdayOnline <#> _.sourceFrom
            , weekdayTo = team.weekdayOnline <#> _.sourceTo
            , weekendFrom = team.weekendOnline <#> _.sourceFrom
            , weekendTo = team.weekendOnline <#> _.sourceTo
            , about = intercalate "\n\n" team.about
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
    -> (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (editTeam :: Slot | children) (Async left)
editTeam input handleMessage = HH.slot
    (SProxy :: SProxy "editTeam") unit
    (Modal.component "Edit team" component) input handleMessage
