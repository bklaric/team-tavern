module TeamTavern.Client.Pages.Player.EditContacts (Input, editContacts) where

import Prelude

import Async (Async)
import Data.Array (nubEq)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.ContactsFormInput (contactsFormInput)
import TeamTavern.Client.Components.Player.ContactsFormInput as ContactsFormInput
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Routes.Player.UpdatePlayerContacts (RequestContent, BadContent)
import TeamTavern.Routes.Player.ViewPlayer (OkContentProfile)
import TeamTavern.Routes.Shared.PlayerContacts as Routes
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input fields = Routes.PlayerContactsOpen (nickname :: String, profiles :: Array OkContentProfile | fields)

data Action
    = UpdateContacts ContactsFormInput.Output
    | Update Event

type State =
    { nickname :: String
    , contacts :: ContactsFormInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

type ChildSlots = (playerContactsFormInput :: ContactsFormInput.Slot)

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { contacts, submitting, otherError } =
    form Update $
    [ contactsFormInput contacts UpdateContacts
    , submitButton "fas fa-edit" "Edit contacts" "Editing contacts..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: ∀ left. State -> Async left $ Maybe $ Either BadContent Unit
sendRequest { nickname, contacts } =
    putNoContent ("/api/players/" <> nickname <> "/contacts") $ (pick contacts :: RequestContent)

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateContacts contacts) =
    H.modify_ _
        { contacts
            { discordTag = contacts.discordTag
            , steamId = contacts.steamId
            , riotId = contacts.riotId
            , battleTag = contacts.battleTag
            , eaId = contacts.eaId
            , ubisoftUsername = contacts.ubisoftUsername
            , psnId = contacts.psnId
            , gamerTag = contacts.gamerTag
            , friendCode = contacts.friendCode
            }
        }
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _
        { submitting = true
        , otherError = false
        , contacts
            { discordTagError = false
            , steamIdError = false
            , riotIdError = false
            , battleTagError = false
            , eaIdError = false
            , ubisoftUsernameError = false
            , psnIdError = false
            , gamerTagError = false
            , friendCodeError = false
            }
        }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right _) -> do
            track_ "Player contacts edit"
            hardNavigate $ "/players/" <> currentState.nickname
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { discordTag: const state { contacts { discordTagError = true } }
                , steamId: const state { contacts { steamIdError = true } }
                , riotId: const state { contacts { riotIdError = true } }
                , battleTag: const state { contacts { battleTagError = true } }
                , eaId: const state { contacts { eaIdError = true } }
                , ubisoftUsername: const state { contacts { ubisoftUsernameError = true } }
                , psnId: const state { contacts { psnIdError = true } }
                , gamerTag: const state { contacts { gamerTagError = true } }
                , friendCode: const state { contacts { friendCodeError = true } }
                }
                error
            )
            (currentState { submitting = false })
            badContent
        Nothing -> H.put currentState { submitting = false, otherError = true }

component :: ∀ query fields output left. H.Component query (Input fields) output (Async left)
component = H.mkComponent
    { initialState: \player ->
        { nickname: player.nickname
        , contacts:
            { requiredPlatforms: player.profiles <#> _.platform # nubEq
            , discordTag: player.discordTag
            , discordTagError: false
            , steamId: player.steamId
            , steamIdError: false
            , riotId: player.riotId
            , riotIdError: false
            , battleTag: player.battleTag
            , battleTagError: false
            , eaId: player.eaId
            , eaIdError: false
            , ubisoftUsername: player.ubisoftUsername
            , ubisoftUsernameError: false
            , psnId: player.psnId
            , psnIdError: false
            , gamerTag: player.gamerTag
            , gamerTagError: false
            , friendCode: player.friendCode
            , friendCodeError: false
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editContacts
    :: ∀ fields action slots left
    .  Input fields
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (editContacts :: Modal.Slot_ | slots) (Async left)
editContacts input handleMessage = HH.slot
    (Proxy :: _ "editContacts") unit
    (Modal.component "Edit contacts" component) input handleMessage
