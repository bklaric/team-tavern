module TeamTavern.Client.Pages.Player (Input, Slot, player) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Control.Monad.State (class MonadState)
import Data.Const (Const)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Ads (descriptionLeaderboards, stickyLeaderboards)
import TeamTavern.Client.Components.Content (contentColumns, contentDescription, contentHeader, contentHeaderSection, contentHeading', contentHeadingFaIcon)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots)
import TeamTavern.Client.Pages.Player.Contacts (contacts)
import TeamTavern.Client.Pages.Player.CreateProfileButton as CreateProfileButton
import TeamTavern.Client.Pages.Player.CreateTeam (createTeam)
import TeamTavern.Client.Pages.Player.CreateTeam as CreateTeam
import TeamTavern.Client.Pages.Player.Details (details)
import TeamTavern.Client.Pages.Player.EditContacts (editContacts)
import TeamTavern.Client.Pages.Player.EditContacts as EditContacts
import TeamTavern.Client.Pages.Player.EditPlayer (editPlayer)
import TeamTavern.Client.Pages.Player.EditPlayer as EditDetails
import TeamTavern.Client.Pages.Player.EditProfile (editProfile)
import TeamTavern.Client.Pages.Player.EditProfile as EditProfile
import TeamTavern.Client.Pages.Player.PlayerOptions (playerOptions)
import TeamTavern.Client.Pages.Player.PlayerOptions as PlayerOptions
import TeamTavern.Client.Pages.Player.Profiles (profiles)
import TeamTavern.Client.Pages.Player.Status (Status(..), getStatus)
import TeamTavern.Client.Pages.Player.Teams (teams)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Slot (QuerylessSlot)
import TeamTavern.Routes.ViewPlayer as ViewPlayer

type Input = { nickname :: String }

type Loaded =
    { player :: ViewPlayer.OkContent
    , status :: Status
    , editContactsModalShown :: Boolean
    , editPlayerModalShown :: Boolean
    , editProfileModalShown :: Maybe ViewPlayer.OkContentProfile
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
    | ShowCreateTeamModal
    | HideCreateTeamModal

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots = PlatformIdSlots
    ( discordTag :: Copyable.Slot String
    , team :: NavigationAnchor.Slot String
    , games :: NavigationAnchor.Slot String
    , editContacts :: EditContacts.Slot
    , editPlayer :: EditDetails.Slot
    , createProfile :: CreateProfileButton.Slot
    , editProfile :: EditProfile.Slot
    , createTeam :: CreateTeam.Slot
    , playerOptions :: PlayerOptions.Slot
    , playerProfileOptions :: QuerylessSlot Unit String
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded state @ { player: player', status }) =
    HH.div_ $
    [ contentHeader $
        [ contentHeaderSection [ contentHeading'
            [ contentHeadingFaIcon "fas fa-user", HH.text player'.nickname ] ]
        ]
        <> guard (status == SignedInSelf) [ playerOptions player'.nickname ]
    , contentDescription
        case status of
        SignedInSelf -> "View and edit all your details, profiles and teams."
        _ -> "View all player's details, profiles and teams."
    ]
    <> descriptionLeaderboards
    <>
    [ contentColumns
        [ HH.div_
            [ contacts player' status ShowEditContactsModal
            , details player' status ShowEditPlayerModal
            ]
        , HH.div_
            [ profiles player' status ShowEditProfileModal
            , teams player' status ShowCreateTeamModal
            ]
        ]
    ]
    <> stickyLeaderboards
    <> guard state.editContactsModalShown [ editContacts player' $ const $ Just HideEditContactsModal ]
    <> guard state.editPlayerModalShown [ editPlayer player' $ const $ Just HideEditPlayerModal ]
    <> guard state.createTeamModalShown [ createTeam $ const $ Just HideCreateTeamModal ]
    <> foldMap
        (\profile -> [ editProfile { player: player', profile } $ const $ Just HideEditProfileModal ])
        state.editProfileModalShown
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> Async left (Maybe ViewPlayer.OkContent)
loadPlayer nickname = do
    timezone <- getClientTimezone
    get ("/api/players/" <> nickname <> "?timezone=" <> timezone)

modifyLoaded :: forall monad. MonadState State monad => (Loaded -> Loaded) -> monad Unit
modifyLoaded mod =
    H.modify_
    case _ of
    Loaded state -> Loaded $ mod state
    state -> state

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive { nickname }) = do
    player' <- H.lift $ loadPlayer nickname
    case player' of
        Just player'' -> do
            status <- getStatus player''.nickname
            H.put $ Loaded
                { player: player''
                , status
                , editContactsModalShown: false
                , editPlayerModalShown: false
                , editProfileModalShown: Nothing
                , createTeamModalShown: false
                }
            setMeta (player''.nickname <> " | TeamTavern")
                ("View all details, profiles and teams of player " <> player''.nickname <> ".")
        _ -> pure unit
handleAction ShowEditContactsModal = modifyLoaded _ { editContactsModalShown = true }
handleAction HideEditContactsModal = modifyLoaded _ { editContactsModalShown = false }
handleAction ShowEditPlayerModal = modifyLoaded _ { editPlayerModalShown = true }
handleAction HideEditPlayerModal = modifyLoaded _ { editPlayerModalShown = false }
handleAction (ShowEditProfileModal profile) = modifyLoaded _ { editProfileModalShown = Just profile }
handleAction HideEditProfileModal = modifyLoaded _ { editProfileModalShown = Nothing }
handleAction ShowCreateTeamModal = modifyLoaded _ { createTeamModalShown = true }
handleAction HideCreateTeamModal = modifyLoaded _ { createTeamModalShown = false }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

player :: forall query children left.
    Input -> HH.ComponentHTML query (player :: Slot | children) (Async left)
player input = HH.slot (SProxy :: SProxy "player") unit component input absurd
