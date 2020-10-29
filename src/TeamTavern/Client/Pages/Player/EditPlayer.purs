module TeamTavern.Client.Pages.Player.EditPlayer (Input, Slot, editPlayer) where

import Prelude

import Async (Async)
import Data.Array (intercalate)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.PlayerFormInput (playerFormInput)
import TeamTavern.Client.Components.Player.PlayerFormInput as EnterPlayerDetails
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Routes.ViewPlayer as ViewPlayer
import TeamTavern.Server.Player.UpdatePlayer.SendResponse as Update
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

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, submitting, otherError } =
    form Update $
    [ HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
        [ HH.text """Enter details about yourself so your new bruh gamer friends
            can find you, bruh. Fill out as much as you can to ensure the
            bruhest gamers find you. All fields are optional, bruh."""
        ]
    , playerFormInput details (Just <<< UpdatePlayerDetails)
    , submitButton "fas fa-edit" "Edit details" "Editing details..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: forall left. State -> Async left (Maybe (Either Update.BadRequestContent Unit))
sendRequest { nickname, details } =
    putNoContent ("/api/players/" <> nickname)
    { birthday: details.birthday
    , location: details.location
    , languages: details.languages
    , microphone: details.microphone
    , discordTag: details.discordTag
    , timezone: details.timezone
    , weekdayFrom: details.weekdayFrom
    , weekdayTo: details.weekdayTo
    , weekendFrom: details.weekendFrom
    , weekendTo: details.weekendTo
    , about: details.about
    }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdatePlayerDetails details) =
    H.modify_ \state -> state
        { details = state.details
            { birthday = details.birthday
            , location = details.location
            , languages = details.languages
            , microphone = details.microphone
            , discordTag = details.discordTag
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right _) -> hardNavigate $ "/players/" <> currentState.nickname
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { invalidDiscordTag: const $ state
                    { details = state.details { discordTagError = true } }
                , invalidAbout: const $ state
                    { details = state.details { aboutError = true } }
                }
                error
            )
            (currentState
                { submitting = false
                , details = currentState.details
                    { discordTagError = false, aboutError = false }
                , otherError = false
                }
            )
            errors
        Nothing -> H.put currentState
            { submitting = false
            , details = currentState.details
                { discordTagError = false, aboutError = false }
            , otherError = true
            }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \player ->
        { nickname: player.nickname
        , details:
            { birthday: player.birthday
            , location: player.location
            , languages: player.languages
            , microphone: player.microphone
            , discordTag: player.discordTag
            , timezone: player.timezone
            , weekdayFrom: player.weekdayOnline <#> _.sourceFrom
            , weekdayTo: player.weekdayOnline <#> _.sourceTo
            , weekendFrom: player.weekendOnline <#> _.sourceFrom
            , weekendTo: player.weekendOnline <#> _.sourceTo
            , about: intercalate "\n\n" player.about
            , discordTagError: false
            , aboutError: false
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editPlayer
    :: forall action children left
    .  Input
    -> (Modal.Output Void -> Maybe action)
    -> HH.ComponentHTML action (editPlayer :: Slot | children) (Async left)
editPlayer input handleMessage = HH.slot
    (SProxy :: SProxy "editPlayer") unit
    (Modal.component "Edit player details" component) input handleMessage
