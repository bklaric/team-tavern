module TeamTavern.Client.Pages.Player.CreateProfile (Input, Slot, createProfile) where

import Prelude

import Async (Async)
import Data.Array (foldl)
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
import TeamTavern.Client.Components.Player.ProfileInputGroup (Field)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (postNoContent)
import TeamTavern.Routes.Shared.ExternalIdIlk (ExternalIdIlks)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { nickname :: String
    , handle :: String
    , title :: String
    , externalIdIlks :: ExternalIdIlks
    , fields :: Array Field
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

type ChildSlots = ("playerProfileFormInput" :: ProfileFormInput.Slot)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, otherError, submitting } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-plus"
        "Create player profile" "Creating player profile..." submitting
    ]
    <> otherFormError otherError

sendRequest
    :: forall left
    .  State
    -> Async left (Maybe
        ( Either
            ( Array (Variant
                ( externalId :: Array String
                , ambitions :: Array String
                , url :: { key :: String, message :: Array String }
                ))
            )
            Unit
        ))
sendRequest state @ { nickname, handle, profile } =
    postNoContent ("/api/players/" <> nickname <> "/profiles/" <> handle)
    { externalId: profile.externalId
    , fieldValues: profile.fieldValues
    , newOrReturning: profile.newOrReturning
    , ambitions: profile.ambitions
    }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateProfile profile) =
    H.modify_ _
        { profile
            { externalIdIlk = profile.externalIdIlk
            , externalId = profile.externalId
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
        Just (Right _) -> hardNavigate $ "/players/" <> currentState.nickname
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { externalId: const state { profile { externalIdError = true } }
                , ambitions: const state { profile { ambitionsError = true } }
                , url: \{ key } -> state { profile
                    { urlErrors = Array.cons key state.profile.urlErrors } }
                }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , profile
                    { externalIdError = false
                    , urlErrors = []
                    , ambitionsError = false
                    }
                }
            )
            badContent
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            , profile
                { externalIdError = false
                , urlErrors = []
                , ambitionsError = false
                }
            }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ nickname, handle, title, externalIdIlks, fields } ->
        { nickname
        , handle
        , title
        , profile: ProfileFormInput.emptyInput { externalIdIlks, fields }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

createProfile
    :: forall children action left
    .  Input
    -> (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (createProfile :: Slot | children) (Async left)
createProfile input handleMessage = HH.slot
    (SProxy :: SProxy "createProfile") unit
    (Modal.component ("Create your " <> input.title <> " profile") component) input handleMessage
