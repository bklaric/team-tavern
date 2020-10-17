module TeamTavern.Client.Pages.Team.CreateProfile (Input, createProfile) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.MultiMap as MultiMap
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, match)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.EnterProfile (enterProfile)
import TeamTavern.Client.Components.Team.EnterProfile as EnterProfile
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Request (postNoContent)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { teamHandle :: String
    , gameHandle :: String
    , title :: String
    , fields :: Array Field
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

type ChildSlots = (enterProfile :: EnterProfile.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, submitting, otherError } =
    form SendRequest $
    [ enterProfile profile UpdateProfile
    , submitButton "fas fa-user-plus" "Create team profile" "Creating team profile..." submitting
    ]
    <>
    otherFormError otherError

sendRequest
    :: forall left
    .  State
    -> Async left (Maybe (Either (Array (Variant (ambitions :: Array String))) Unit))
sendRequest state @ { teamHandle, gameHandle, profile } =
    postNoContent ("/api/teams/" <> teamHandle <> "/profiles/" <> gameHandle)
    { fieldValues:
        profile.fieldValues
        # (MultiMap.toUnfoldable :: _ -> Array _)
        <#> \(Tuple fieldKey optionKeys) ->
            { fieldKey
            , optionKeys: Array.fromFoldable optionKeys
            }
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
        Just (Right _) -> navigate_ $ "/teams/" <> currentState.teamHandle
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
    { initialState: \{ teamHandle, gameHandle, title, fields } ->
        { teamHandle
        , gameHandle
        , title
        , profile: EnterProfile.emptyInput fields
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
