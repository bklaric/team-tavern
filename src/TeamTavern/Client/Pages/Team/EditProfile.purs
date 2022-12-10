module TeamTavern.Client.Pages.Team.EditProfile where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Form (form, otherFormError, submitButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Team.ProfileFormInput (profileFormInput)
import TeamTavern.Client.Components.Team.ProfileFormInput as EnterProfile
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.Request (putNoContent)
import TeamTavern.Routes.Profile.AddTeamProfile as AddTeamProfile
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { team :: ViewTeam.OkContent
    , profile :: ViewTeam.OkContentProfile
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

type ChildSlots = (teamProfileFormInput :: EnterProfile.Slot)

type Slot = H.Slot (Const Void) (Modal.Output Void) Unit

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profile, submitting, otherError } =
    form SendRequest $
    [ profileFormInput profile UpdateProfile
    , submitButton "fas fa-user-edit" "Edit team profile" "Editting team profile..." submitting
    ]
    <>
    otherFormError otherError

sendRequest :: ∀ left. State -> Async left $ Maybe $ Either AddTeamProfile.BadContent Unit
sendRequest { teamHandle, gameHandle, profile } = let
    details = profile.details
        # Record.insert (Proxy :: _ "platforms") profile.details.selectedPlatforms
        # pick
    in
    putNoContent ("/api/teams/" <> teamHandle <> "/profiles/" <> gameHandle)
    ({ details
    , contacts: pick profile.contacts
    } :: AddTeamProfile.RequestContent)

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (UpdateProfile profile) =
    H.modify_ _
        { profile
            { details
                { size = profile.details.size
                , selectedPlatforms = profile.details.platforms
                , fieldValues = profile.details.fieldValues
                , newOrReturning = profile.details.newOrReturning
                , about = profile.details.about
                , ambitions = profile.details.ambitions
                }
            , contacts
                { discordTag = profile.contacts.discordTag
                , discordServer = profile.contacts.discordServer
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
                { platformsError = false
                , aboutError = false
                }
            , contacts
                { discordTagError = false
                , discordServerError = false
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
        Just (Right _) -> hardNavigate $ "/teams/" <> currentState.teamHandle
        Just (Left badContent) -> H.put $
            foldl
            (\state error ->
                match
                { profile: state # foldl \state' error' -> error' # match
                    { platforms: const state' { profile { details { platformsError = true } } }
                    , about: const state' { profile { details { aboutError = true } } }
                    , ambitions: const state' { profile { details { ambitionsError = true } } }
                    }
                , contacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { profile { contacts { discordTagError = true } } }
                    , discordServer: const state' { profile { contacts { discordServerError = true } } }
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

component :: ∀ query output left.
    H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: \state @
        { team:
            { discordTag, discordServer
            , steamId, riotId, battleTag, eaId, ubisoftUsername, psnId, gamerTag, friendCode
            }
        , profile: { title }
        } ->
        { teamHandle: state.team.handle
        , gameHandle: state.profile.handle
        , title
        , profile:
            { details:
                { size: state.profile.size
                , allPlatforms: state.profile.allPlatforms
                , selectedPlatforms: state.profile.selectedPlatforms
                , platformsError: false
                , fields: state.profile.fields
                , fieldValues: state.profile.fieldValues
                , newOrReturning: state.profile.newOrReturning
                , about: intercalate "\n\n" state.profile.about
                , aboutError: false
                , ambitions: intercalate "\n\n" state.profile.ambitions
                , ambitionsError: false
                }
            , contacts:
                { discordTag
                , discordTagError: false
                , discordServer
                , discordServerError: false
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
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

editProfile
    :: ∀ action children left
    .  Input
    -> (Modal.Output Void -> action)
    -> HH.ComponentHTML action (editProfile :: Slot | children) (Async left)
editProfile input handleMessage = HH.slot
    (Proxy :: _ "editProfile") unit
    (Modal.component ("Edit " <> input.profile.title <> " team profile") component) input handleMessage
