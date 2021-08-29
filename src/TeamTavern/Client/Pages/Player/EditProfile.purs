module TeamTavern.Client.Pages.Player.EditProfile where

import Prelude

import Async (Async)
import Data.Array (foldl, intercalate)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), match)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.ProfileFormInput (profileFormInput)
import TeamTavern.Client.Components.Player.ProfileFormInput as ProfileFormInput
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Routes.ViewPlayer as ViewPlayer
import TeamTavern.Server.Profile.UpdatePlayerProfile.SendResponse (BadContent)
import Type (type ($))
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

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

type ChildSlots = ("playerProfileFormInput" :: ProfileFormInput.Slot)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, otherError, submitting } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-plus"
        "Edit player profile" "Editting player profile..." submitting
    ]
    <> otherFormError otherError

sendRequest :: forall left. State -> Async left $ Maybe $ Either BadContent Unit
sendRequest state @ { nickname, handle, profile } =
    putNoContent ("/api/players/" <> nickname <> "/profiles/" <> handle)
    { platform: profile.platform
    , contacts: profile.contacts
    , fieldValues: profile.fieldValues
    , newOrReturning: profile.newOrReturning
    , about: profile.about
    }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateProfile profile) =
    H.modify_ _
        { profile
            { platform = profile.platform
            , contacts
                { discordTag = profile.contacts.discordTag
                , steamId = profile.contacts.steamId
                , riotId = profile.contacts.riotId
                , battleTag = profile.contacts.battleTag
                , psnId = profile.contacts.psnId
                , gamerTag = profile.contacts.gamerTag
                , friendCode = profile.contacts.friendCode
                }
            , fieldValues = profile.fieldValues
            , newOrReturning = profile.newOrReturning
            , about = profile.about
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
                { profile: state # foldl \state' error' -> error' # match
                    { about: const state { profile { aboutError = true } }
                    , url: \{ key } -> state { profile
                        { urlErrors = Array.cons key state.profile.urlErrors } }
                    }
                , contacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { profile { contacts { discordTagError = true } } }
                    , steamId: const state' { profile { contacts { steamIdError = true } } }
                    , riotId: const state' { profile { contacts { riotIdError = true } } }
                    , battleTag: const state' { profile { contacts { battleTagError = true } } }
                    , psnId: const state' { profile { contacts { psnIdError = true } } }
                    , gamerTag: const state' { profile { contacts { gamerTagError = true } } }
                    , friendCode: const state' { profile { contacts { friendCodeError = true } } }
                    }
                }
                error
            )
            (currentState
                { submitting = false
                , otherError = false
                , profile
                    { urlErrors = []
                    , aboutError = false
                    }
                }
            )
            badContent
        Nothing -> H.put currentState
            { submitting = false
            , otherError = true
            , profile
                { urlErrors = []
                , aboutError = false
                }
            }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \
        { player: { nickname, discordTag, steamId, riotId, battleTag, psnId, gamerTag, friendCode }
        , profile: { handle, title, platforms, fields, platform, fieldValues, newOrReturning, about }
        } ->
        { nickname
        , handle
        , title
        , profile:
            { platforms
            , fields
            , platform: platforms.head
            , contacts:
                { discordTag
                , discordTagError: false
                , steamId
                , steamIdError: false
                , riotId
                , riotIdError: false
                , battleTag
                , battleTagError: false
                , psnId
                , psnIdError: false
                , gamerTag
                , gamerTagError: false
                , friendCode
                , friendCodeError: false
                }
            , fieldValues
            , newOrReturning
            , about: intercalate "\n\n" about
            , urlErrors: []
            , aboutError: false
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
editProfile input handleMessage =
    HH.slot
    (SProxy :: SProxy "editProfile") unit
    (Modal.component ("Edit your " <> input.profile.title <> " profile") component)
    input handleMessage
