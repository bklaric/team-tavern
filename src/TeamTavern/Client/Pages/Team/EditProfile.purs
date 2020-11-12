module TeamTavern.Client.Pages.Team.EditProfile where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, match)
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.ProfileFormInput (FieldValues, profileFormInput)
import TeamTavern.Client.Components.Team.ProfileFormInput as EnterProfile
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { teamHandle :: String
    , gameHandle :: String
    , title :: String
    , fields :: Array Field
    , profile ::
        { fieldValues :: FieldValues
        , newOrReturning :: Boolean
        , ambitions :: String
        }
    }

type State =
    { teamHandle :: String
    , gameHandle :: String
    , title :: String
    , profile :: EnterProfile.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = UpdateProfile EnterProfile.Output
    | SendRequest Event

type ChildSlots = (teamProfileFormInput :: EnterProfile.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, submitting, otherError } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-edit" "Edit team profile" "Editting team profile..." submitting
    ]
    <>
    otherFormError otherError

sendRequest
    :: forall left
    .  State
    -> Async left (Maybe (Either (Array (Variant (ambitions :: Array String))) Unit))
sendRequest state @ { teamHandle, gameHandle, profile } =
    putNoContent ("/api/teams/" <> teamHandle <> "/profiles/" <> gameHandle)
    { fieldValues: profile.fieldValues
    , newOrReturning: profile.newOrReturning
    , ambitions: profile.ambitions
    }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateProfile profile) =
    H.modify_ \state -> state
        { profile = state.profile
            { fieldValues = profile.fieldValues
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
                { ambitions: const state { profile = state.profile { ambitionsError = true } } }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , profile = currentState.profile
                    { ambitionsError = false }
                }
            )
            badContent
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            , profile = currentState.profile
                { ambitionsError = false }
            }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ teamHandle, gameHandle, title, fields, profile } ->
        { teamHandle
        , gameHandle
        , title
        , profile: profile
            # Record.insert (SProxy :: SProxy "ambitionsError") false
            # Record.insert (SProxy :: SProxy "fields") fields
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

editProfile
    :: forall action children left
    .  Input
    -> (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (editProfile :: Slot | children) (Async left)
editProfile input handleMessage = HH.slot
    (SProxy :: SProxy "editProfile") unit
    (Modal.component ("Edit " <> input.title <> " team profile") component) input handleMessage
