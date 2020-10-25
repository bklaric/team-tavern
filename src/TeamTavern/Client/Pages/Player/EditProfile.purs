module TeamTavern.Client.Pages.Player.EditProfile where

import Prelude

import Async (Async)
import Data.Array (foldl, intercalate)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, match)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.ProfileFormInput (profileFormInput)
import TeamTavern.Client.Components.Player.ProfileFormInput as ProfileFormInput
import TeamTavern.Client.Components.Player.ProfileInputGroup (Field, FieldValue)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { nickname :: String
    , handle :: String
    , title :: String
    , fields :: Array Field
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , ambitions :: Array String
    }

type State =
    { nickname :: String
    , handle :: String
    , title :: String
    , profile :: ProfileFormInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = UpdateProfile ProfileFormInput.Output
    | SendRequest Event

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

type ChildSlots = ("profileFormInput" :: ProfileFormInput.Slot)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, otherError, submitting } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-plus"
        "Edit player profile" "Editting player profile..." submitting
    ]
    <> otherFormError otherError

sendRequest
    :: forall left
    .  State
    -> Async left (Maybe
        ( Either
            ( Array (Variant
                ( ambitions :: {}
                , url :: { key :: String, message :: Array String }
                , missing :: { key :: String, message :: Array String }
                ))
            )
            Unit
        ))
sendRequest state @ { nickname, handle, profile } =
    putNoContent ("/api/players/" <> nickname <> "/profiles/" <> handle)
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
        Just (Right _) -> hardNavigate $ "/players/" <> currentState.nickname
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { ambitions: const state { profile { ambitionsError = true } }
                , url: \{ key } -> state { profile
                    { urlErrors = Array.cons key state.profile.urlErrors } }
                , missing: \{ key } -> state { profile
                    { missingErrors = Array.cons key state.profile.missingErrors } }
                }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , profile
                    { urlErrors = []
                    , missingErrors = []
                    , ambitionsError = false
                    }
                }
            )
            badContent
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            , profile
                { urlErrors = []
                , missingErrors = []
                , ambitionsError = false
                }
            }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ nickname, handle, title, fields, fieldValues, newOrReturning, ambitions } ->
        { nickname
        , handle
        , title
        , profile: (ProfileFormInput.emptyInput fields)
            { fieldValues = fieldValues
            , newOrReturning = newOrReturning
            , ambitions = intercalate "\n\n" ambitions
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editProfile
    :: forall query children left
    .  Input
    -> (Modal.Output Void -> Maybe query)
    -> HH.ComponentHTML query (editProfile :: Slot | children) (Async left)
editProfile input handleMessage = HH.slot
    (SProxy :: SProxy "editProfile") unit
    (Modal.component ("Edit your " <> input.title <> " profile") component) input handleMessage
