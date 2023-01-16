module TeamTavern.Client.Pages.Player.EditProfile where

import Prelude

import Async (Async)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.ProfileFormInput (profileFormInput)
import TeamTavern.Client.Components.Player.ProfileFormInput as ProfileFormInput
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { player :: ViewPlayer.OkContent
    , profile :: ViewPlayer.OkContentProfile
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

type ChildSlots = ("playerProfileFormInput" :: ProfileFormInput.Slot)

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, otherError, submitting } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-plus"
        "Edit player profile" "Editting player profile..." submitting
    ]
    <> otherFormError otherError

sendRequest :: ∀ left. State -> Async left $ Maybe $ Either AddPlayerProfile.BadContent Unit
sendRequest { nickname, handle, profile } =
    putNoContent ("/api/players/" <> nickname <> "/profiles/" <> handle)
    ({ details: pick profile.details
    , contacts: pick profile.contacts
    } :: AddPlayerProfile.RequestContent)

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateProfile profile) =
    H.modify_ _
        { profile
            { details
                { platform = profile.details.platform
                , fieldValues = profile.details.fieldValues
                , newOrReturning = profile.details.newOrReturning
                , about = profile.details.about
                , ambitions = profile.details.ambitions
                }
            , contacts
                { discordTag = profile.contacts.discordTag
                , steamId = profile.contacts.steamId
                , riotId = profile.contacts.riotId
                , battleTag = profile.contacts.battleTag
                , eaId = profile.contacts.eaId
                , ubisoftUsername = profile.contacts.ubisoftUsername
                , psnId = profile.contacts.psnId
                , gamerTag = profile.contacts.gamerTag
                , friendCode = profile.contacts.friendCode
                }
            }
        }
handleAction (SendRequest event) = do
    H.liftEffect $ preventDefault event
    currentState <- H.modify _
        { submitting = true
        , otherError = false
        , profile
            { details
                { urlErrors = []
                , aboutError = false
                }
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
        }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right _) -> do
            track "Profile edit" {ilk: "player", game: currentState.handle}
            hardNavigate $ "/players/" <> currentState.nickname
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { profile: state # foldl \state' error' -> error' # match
                    { about: const state' { profile { details { aboutError = true } } }
                    , ambitions: const state' { profile { details { ambitionsError = true } } }
                    , url: \{ key } -> state' { profile { details
                        { urlErrors = Array.cons key state'.profile.details.urlErrors } } }
                    }
                , contacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { profile { contacts { discordTagError = true } } }
                    , steamId: const state' { profile { contacts { steamIdError = true } } }
                    , riotId: const state' { profile { contacts { riotIdError = true } } }
                    , battleTag: const state' { profile { contacts { battleTagError = true } } }
                    , eaId: const state' { profile { contacts { eaIdError = true } } }
                    , ubisoftUsername: const state' { profile { contacts { ubisoftUsernameError = true } } }
                    , psnId: const state' { profile { contacts { psnIdError = true } } }
                    , gamerTag: const state' { profile { contacts { gamerTagError = true } } }
                    , friendCode: const state' { profile { contacts { friendCodeError = true } } }
                    }
                }
                error
            )
            (currentState { submitting = false })
            badContent
        Nothing -> H.put currentState { submitting = false, otherError = true }

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: \
        { player: { nickname, discordTag, steamId, riotId, battleTag, eaId, ubisoftUsername, psnId, gamerTag, friendCode }
        , profile: { handle, title, platforms, fields, platform, fieldValues, newOrReturning, about, ambitions }
        } ->
        { nickname
        , handle
        , title
        , profile:
            { details:
                { platforms
                , fields
                , platform
                , fieldValues
                , newOrReturning
                , about: intercalate "\n\n" about
                , aboutError: false
                , ambitions: intercalate "\n\n" ambitions
                , ambitionsError: false
                , urlErrors: []
                }
            , contacts:
                { discordTag
                , discordTagError: false
                , steamId
                , steamIdError: false
                , riotId
                , riotIdError: false
                , battleTag
                , battleTagError: false
                , eaId
                , eaIdError: false
                , ubisoftUsername
                , ubisoftUsernameError: false
                , psnId
                , psnIdError: false
                , gamerTag
                , gamerTagError: false
                , friendCode
                , friendCodeError: false
                }
            }
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editProfile
    :: ∀ query children left
    .  Input
    -> (Modal.Output Void -> query)
    -> HH.ComponentHTML query (editProfile :: Modal.Slot_ | children) (Async left)
editProfile input handleMessage =
    HH.slot
    (Proxy :: _ "editProfile") unit
    (Modal.component ("Edit your " <> input.profile.title <> " profile") component)
    input handleMessage
