module TeamTavern.Client.Pages.Player.EditPlayer (Input, editPlayer) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.PlayerFormInput (playerFormInput)
import TeamTavern.Client.Components.Player.PlayerFormInput as EnterPlayerDetails
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent')
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = ViewPlayer.OkContent

data Action
    = UpdatePlayerDetails EnterPlayerDetails.Output
    | Update Event

type State =
    { nickname :: String
    , details :: EnterPlayerDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

type ChildSlots = (playerFormInput :: EnterPlayerDetails.Slot)

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    form Update $
    [ playerFormInput details UpdatePlayerDetails
    , submitButton "fas fa-edit" "Edit player" "Editing player..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: ∀ left. State -> Async left (Maybe Unit)
sendRequest { nickname, details } =
    putNoContent' ("/api/players/" <> nickname)
    { birthday: details.birthday
    , location: details.location
    , languages: details.languages
    , microphone: details.microphone
    , timezone: details.timezone
    , weekdayFrom: details.weekdayFrom
    , weekdayTo: details.weekdayTo
    , weekendFrom: details.weekendFrom
    , weekendTo: details.weekendTo
    }

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdatePlayerDetails details) =
    H.modify_ \state -> state
        { details = state.details
            { birthday = details.birthday
            , location = details.location
            , languages = details.languages
            , microphone = details.microphone
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            }
        }
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just _ -> do
            track_ "Player edit"
            hardNavigate $ "/players/" <> currentState.nickname
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            }

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: \player ->
        { nickname: player.nickname
        , details:
            { birthday: player.birthday
            , location: player.location
            , languages: player.languages
            , microphone: player.microphone
            , timezone: player.timezone
            , weekdayFrom: player.weekdayOnline <#> _.sourceFrom
            , weekdayTo: player.weekdayOnline <#> _.sourceTo
            , weekendFrom: player.weekendOnline <#> _.sourceFrom
            , weekendTo: player.weekendOnline <#> _.sourceTo
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editPlayer
    :: ∀ action children left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (editPlayer :: Modal.Slot_ | children) (Async left)
editPlayer input handleMessage = HH.slot
    (Proxy :: _ "editPlayer") unit
    (Modal.component "Edit player" component) input handleMessage
