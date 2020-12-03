module TeamTavern.Client.Pages.Player.EditSettings (Input, Slot, editSettings) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Input (checkboxInput, inputGroup, inputLabel)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent')
import TeamTavern.Routes.ViewPlayer as ViewPlayer
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = ViewPlayer.OkContent

data Action
    = UpdateNotify Boolean
    | Update Event

type State =
    { nickname :: String
    , notify :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall slots left. State -> H.ComponentHTML Action slots (Async left)
render { notify, otherError, submitting } =
    form Update $
    [ inputGroup
        [ inputLabel "fas fa-envelope-open-text" "Message notifications"
        , checkboxInput notify UpdateNotify "Send me an email when someone messages me."
        ]
    , submitButton "fas fa-edit" "Edit settings" "Editing settings..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe Unit)
sendRequest { nickname, notify } =
    putNoContent' ("/api/players/" <> nickname <> "/settings") { notify }

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction (UpdateNotify notify) = H.modify_ _ { notify = notify }
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just _ -> hardNavigate $ "/players/" <> currentState.nickname
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ nickname, notify } ->
        { nickname
        , notify
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editSettings
    :: forall action children left
    .  Input
    -> (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (editSettings :: Slot | children) (Async left)
editSettings input handleMessage = HH.slot
    (SProxy :: SProxy "editSettings") unit
    (Modal.component "Edit your account settings" component) input handleMessage
