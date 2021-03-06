module TeamTavern.Client.Pages.Team.CreateProfile (Input, createProfile) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, match)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.ProfileFormInput (profileFormInput)
import TeamTavern.Client.Components.Team.ProfileFormInput as ProfileFormInput
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (postNoContent)
import TeamTavern.Routes.Shared.Platform (Platforms)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { teamHandle :: String
    , gameHandle :: String
    , title :: String
    , platforms :: Platforms
    , fields :: Array Field
    }

type State =
    { teamHandle :: String
    , gameHandle :: String
    , title :: String
    , profile :: ProfileFormInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = UpdateProfile ProfileFormInput.Output
    | SendRequest Event

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

type ChildSlots = (teamProfileFormInput :: ProfileFormInput.Slot)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, submitting, otherError } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-plus" "Create team profile" "Creating team profile..." submitting
    ]
    <>
    otherFormError otherError

sendRequest
    :: forall left
    .  State
    -> Async left (Maybe (Either (Array (Variant (platforms :: Array String, ambitions :: Array String))) Unit))
sendRequest state @ { teamHandle, gameHandle, profile } =
    postNoContent ("/api/teams/" <> teamHandle <> "/profiles/" <> gameHandle)
    { platforms: profile.selectedPlatforms
    , fieldValues: profile.fieldValues
    , newOrReturning: profile.newOrReturning
    , ambitions: profile.ambitions
    }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateProfile profile) =
    H.modify_ _
        { profile
            { selectedPlatforms = profile.platforms
            , fieldValues = profile.fieldValues
            , newOrReturning = profile.newOrReturning
            , ambitions = profile.ambitions
            }
        }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right _) -> hardNavigate $ "/teams/" <> currentState.teamHandle
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { platforms: const state { profile = state.profile { platformsError = true } }
                , ambitions: const state { profile = state.profile { ambitionsError = true } }
                }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , profile { ambitionsError = false }
                }
            )
            badContent
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            , profile { ambitionsError = false }
            }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ platforms, teamHandle, gameHandle, title, fields } ->
        { teamHandle
        , gameHandle
        , title
        , profile: ProfileFormInput.emptyInput { platforms, fields }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

createProfile
    :: forall action children left
    .  Input
    -> (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (createProfile :: Slot | children) (Async left)
createProfile input handleMessage = HH.slot
    (SProxy :: SProxy "createProfile") unit
    (Modal.component ("Create " <> input.title <> " team profile") component) input handleMessage
