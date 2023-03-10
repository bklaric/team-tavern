module TeamTavern.Client.Pages.Player (Input, player) where

import Prelude

import Async (Async)
import Async as Async
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Ads (mobileMpu)
import TeamTavern.Client.Components.Content (actualContent, contentColumns, contentDescription, contentHeader, contentHeaderSection, contentHeading', contentHeadingFaIcon)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots)
import TeamTavern.Client.Pages.Player.Contacts (contacts)
import TeamTavern.Client.Pages.Player.CreateTeam (createTeam)
import TeamTavern.Client.Pages.Player.DeletePlayerProfile (deletePlayerProfile)
import TeamTavern.Client.Pages.Player.Details (details)
import TeamTavern.Client.Pages.Player.EditContacts (editContacts)
import TeamTavern.Client.Pages.Player.EditPlayer (editPlayer)
import TeamTavern.Client.Pages.Player.EditProfile (editProfile)
import TeamTavern.Client.Pages.Player.PlayerOptions (playerOptions)
import TeamTavern.Client.Pages.Player.PlayerProfileOptions as PlayerProfileOptions
import TeamTavern.Client.Pages.Player.Profiles (profiles)
import TeamTavern.Client.Pages.Player.Status (Status(..), getStatus)
import TeamTavern.Client.Pages.Player.Teams (teams)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Rendertron (appendRendetronNotFound)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPathQuery)
import TeamTavern.Client.Shared.Slot (Slot___, Slot__String)
import TeamTavern.Routes.Player.ViewPlayer (ViewPlayer)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import Type.Proxy (Proxy(..))

type Input = { nickname :: String }

type Loaded =
    { player :: ViewPlayer.OkContent
    , status :: Status
    , editContactsModalShown :: Boolean
    , editPlayerModalShown :: Boolean
    , editProfileModalShown :: Maybe ViewPlayer.OkContentProfile
    , deleteProfileModalShown :: Maybe ViewPlayer.OkContentProfile
    , createTeamModalShown :: Boolean
    }

data State
    = Empty Input
    | Loaded Loaded
    | NotFound
    | Error

data Action
    = Initialize
    | Receive Input
    | ShowEditContactsModal
    | HideEditContactsModal
    | ShowEditPlayerModal
    | HideEditPlayerModal
    | ShowEditProfileModal ViewPlayer.OkContentProfile
    | HideEditProfileModal
    | ShowDeleteProfileModal ViewPlayer.OkContentProfile
    | HideDeleteProfileModal
    | ShowCreateTeamModal
    | HideCreateTeamModal

type ChildSlots = PlatformIdSlots
    ( discordTag :: Slot__String
    , team :: Slot__String
    , games :: Slot__String
    , editContacts :: Modal.Slot_
    , editPlayer :: Modal.Slot_
    , createProfile :: Slot___
    , editProfile :: Modal.Slot_
    , deletePlayerProfile :: Modal.Slot_
    , createTeam :: Modal.Slot_
    , playerOptions :: Slot___
    , playerProfileOptions :: PlayerProfileOptions.Slot
    )

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded state @ { player: player', status }) =
    actualContent $
    [ contentHeader $
        [ contentHeaderSection [ contentHeading'
            [ contentHeadingFaIcon "fas fa-user", HH.text player'.nickname ] ]
        ]
        <> guard (status == SignedInSelf) [ playerOptions $ pick player' ]
    , contentDescription
        case status of
        SignedInSelf -> "View and edit all your details, profiles and teams."
        _ -> "View all player's details, profiles and teams."
    ]
    <>
    [ contentColumns
        [ HH.div_
            [ contacts player' status ShowEditContactsModal
            , details player' status ShowEditPlayerModal
            ]
        , mobileMpu
        , HH.div_
            [ profiles player' status ShowEditProfileModal ShowDeleteProfileModal
            , teams player' status ShowCreateTeamModal
            ]
        ]
    ]
    <> guard state.editContactsModalShown [ editContacts player' $ const HideEditContactsModal ]
    <> guard state.editPlayerModalShown [ editPlayer player' $ const HideEditPlayerModal ]
    <> guard state.createTeamModalShown [ createTeam $ const HideCreateTeamModal ]
    <> foldMap
        (\profile -> [ editProfile { player: player', profile } $ const HideEditProfileModal ])
        state.editProfileModalShown
    <> foldMap
        (\profile -> [ deletePlayerProfile { player: player', profile } $ const HideDeleteProfileModal ])
        state.deleteProfileModalShown
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

modifyLoaded :: ∀ monad. MonadState State monad => (Loaded -> Loaded) -> monad Unit
modifyLoaded mod =
    H.modify_
    case _ of
    Loaded state -> Loaded $ mod state
    state -> state

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) = do
    timezone <- getClientTimezone
    result <- H.lift $ Async.attempt $
        fetchPathQuery (Proxy :: _ ViewPlayer) input { timezone }
    case result of
        Left _ -> H.put Error
        Right response -> response # onMatch
            { ok: \player' -> do
                status <- getStatus player'.nickname
                H.put $ Loaded
                    { player: player'
                    , status
                    , editContactsModalShown: false
                    , editPlayerModalShown: false
                    , editProfileModalShown: Nothing
                    , deleteProfileModalShown: Nothing
                    , createTeamModalShown: false
                    }
                setMeta (player'.nickname <> " | TeamTavern")
                    ("View all details, profiles and teams of player " <> player'.nickname <> ".")
            , notFound: const do
                appendRendetronNotFound
                H.put NotFound
            }
            (const $ H.put Error)
handleAction ShowEditContactsModal = modifyLoaded _ { editContactsModalShown = true }
handleAction HideEditContactsModal = modifyLoaded _ { editContactsModalShown = false }
handleAction ShowEditPlayerModal = modifyLoaded _ { editPlayerModalShown = true }
handleAction HideEditPlayerModal = modifyLoaded _ { editPlayerModalShown = false }
handleAction (ShowEditProfileModal profile) = modifyLoaded _ { editProfileModalShown = Just profile }
handleAction HideEditProfileModal = modifyLoaded _ { editProfileModalShown = Nothing }
handleAction (ShowDeleteProfileModal profile) = modifyLoaded _ { deleteProfileModalShown = Just profile}
handleAction HideDeleteProfileModal = modifyLoaded _ { deleteProfileModalShown = Nothing }
handleAction ShowCreateTeamModal = modifyLoaded _ { createTeamModalShown = true }
handleAction HideCreateTeamModal = modifyLoaded _ { createTeamModalShown = false }

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

player :: ∀ query children left.
    Input -> HH.ComponentHTML query (player :: Slot___ | children) (Async left)
player input = HH.slot (Proxy :: _ "player") unit component input absurd
