module TeamTavern.Client.Pages.Player.DeleteAccount where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (deleteNoContent)
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = String

type State = { nickname :: String, submitting :: Boolean, otherError :: Boolean }

data Action = SendRequest Event

render :: ∀ slots. State -> HH.HTML slots Action
render { submitting, otherError } =
    form SendRequest $
    [ HH.p [ HS.class_ "boarding-description" ]
        [ HH.text $ "Are you sure you want to delete your TeamTavern account? "
        <> "This will delete everything, including player profiles, teams and team profiles." ]
    , submitButton "fas fa-trash" "Delete my account" "Deleting account..." submitting
    ]
    <> otherFormError otherError

sendRequest :: ∀ bad. State -> Async bad (Maybe Unit)
sendRequest { nickname } = deleteNoContent $ "/api/players/" <> nickname

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just _ -> hardNavigate "/"
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            }

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: \nickname ->
        { nickname
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

deleteAccount
    :: ∀ children action left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (deleteAccount :: Modal.Slot_ | children) (Async left)
deleteAccount input handleMessage = HH.slot
    (Proxy :: _ "deleteAccount") unit
    (Modal.component ("Delete your TeamTavern account") component) input handleMessage
