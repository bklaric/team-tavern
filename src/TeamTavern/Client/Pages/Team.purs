module TeamTavern.Client.Pages.Team (Input, Slot, team) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Control.Monad.State (class MonadState)
import Data.Const (Const)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Ads (descriptionLeaderboards, stickyLeaderboards)
import TeamTavern.Client.Components.Content (contentColumns, contentDescription, contentHeader, contentHeaderSection, contentHeading', contentHeadingFaIcon)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots)
import TeamTavern.Client.Pages.Profiles.TeamBadge (informalBadge, organizedBadge)
import TeamTavern.Client.Pages.Team.Contacts (contacts)
import TeamTavern.Client.Pages.Team.DeleteTeamProfile (deleteTeamProfile)
import TeamTavern.Client.Pages.Team.DeleteTeamProfile as DeleteTeamProfile
import TeamTavern.Client.Pages.Team.Details (details)
import TeamTavern.Client.Pages.Team.EditContacts (editContacts)
import TeamTavern.Client.Pages.Team.EditContacts as EditContacts
import TeamTavern.Client.Pages.Team.EditProfile (editProfile)
import TeamTavern.Client.Pages.Team.EditProfile as EditProfile
import TeamTavern.Client.Pages.Team.EditTeam (editTeam)
import TeamTavern.Client.Pages.Team.EditTeam as EditTeam
import TeamTavern.Client.Pages.Team.Profiles (profiles)
import TeamTavern.Client.Pages.Team.Status (Status(..), getStatus)
import TeamTavern.Client.Pages.Team.TeamOptions (teamOptions)
import TeamTavern.Client.Pages.Team.TeamProfileOptions as TeamProfileOptions
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Routes.Shared.Organization (OrganizationNW(..), nameOrHandleNW)
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import Type.Proxy (Proxy(..))

type Input = { handle :: String }

type Loaded =
    { team :: ViewTeam.OkContent
    , status :: Status
    , editContactsModalShown :: Boolean
    , editTeamModalShown :: Boolean
    , editProfileModalShown :: Maybe ViewTeam.OkContentProfile
    , deleteProfileModalShown :: Maybe ViewTeam.OkContentProfile
    }

data State
    = Empty Input
    | Loaded Loaded
    | NotFound
    | Error

data Action
    = Initialize
    | ShowEditContactsModal
    | HideEditContactsModal
    | ShowEditTeamModal
    | HideEditTeamModal
    | ShowEditProfileModal ViewTeam.OkContentProfile
    | HideEditProfileModal
    | ShowDeleteProfileModal ViewTeam.OkContentProfile
    | HideDeleteProfileModal

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots = PlatformIdSlots
    ( discordTag :: Copyable.Slot String
    , games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    , editContacts :: EditContacts.Slot
    , editTeam :: EditTeam.Slot
    , editProfile :: EditProfile.Slot
    , deleteTeamProfile :: DeleteTeamProfile.Slot
    , viewTeamOwner :: Anchor.Slot Unit
    , teamProfileOptions :: TeamProfileOptions.Slot
    , teamOptions :: SimpleSlot
    )

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded state @ { team: team', status } ) =
    HH.div_  $
    [ contentHeader $
        [ contentHeaderSection
            [ contentHeading'
                [ contentHeadingFaIcon "fas fa-users"
                , HH.text $ nameOrHandleNW team'.handle team'.organization
            ]
            , case team'.organization of
                InformalNW -> informalBadge
                OrganizedNW _ -> organizedBadge
            ]
        , case status of
            SignedInOwner -> teamOptions team'
            _ -> navigationAnchor (Proxy :: _ "viewTeamOwner")
                { path: "/players/" <> team'.owner
                , content: HH.span [ HP.style "font-weight: 500" ] [ HH.text "View team owner" ]
                }
        ]
    , contentDescription
        case status of
        SignedInOwner -> "View and edit all your team's details and profiles."
        _ -> "View all team's details and profiles."
    ]
    <> descriptionLeaderboards
    <>
    [ contentColumns
        [ HH.div_
            [ contacts team' status ShowEditContactsModal
            , details team' status ShowEditTeamModal
            ]
        , HH.div_
            [ profiles team' status ShowEditProfileModal ShowDeleteProfileModal ]
        ]
    ]
    <> stickyLeaderboards
    <> guard state.editContactsModalShown [ editContacts team' $ const HideEditContactsModal ]
    <> guard state.editTeamModalShown [ editTeam team' (const HideEditTeamModal) ]
    <> foldMap (\profile ->
        [ editProfile { team: team', profile } (const HideEditProfileModal)
        ])
        state.editProfileModalShown
    <> foldMap
        (\profile -> [ deleteTeamProfile { team: team', profile } $ const HideDeleteProfileModal ])
        state.deleteProfileModalShown
render NotFound = HH.p_ [ HH.text "Team could not be found." ]
render Error = HH.p_ [ HH.text "There has been an error loading the team. Please try again later." ]

loadTeam :: ∀ left. String -> Async left (Maybe ViewTeam.OkContent)
loadTeam handle = do
    timezone <- getClientTimezone
    get $ "/api/teams/" <> handle <> "?timezone=" <> timezone

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
        Empty { handle } -> do
            team' <- H.lift $ loadTeam handle
            case team' of
                Just team'' -> do
                    status <- getStatus team''.owner
                    H.put $ Loaded
                        { team: team''
                        , status
                        , editContactsModalShown: false
                        , editTeamModalShown: false
                        , editProfileModalShown: Nothing
                        , deleteProfileModalShown: Nothing
                        }
                    let nameOrHandle = nameOrHandleNW team''.handle team''.organization
                    setMeta (nameOrHandle <> " | TeamTavern")
                        ("View all details and profiles of team " <> nameOrHandle <> ".")
                _ -> pure unit
        _ -> pure unit
handleAction ShowEditContactsModal = modifyLoaded _ { editContactsModalShown = true }
handleAction HideEditContactsModal = modifyLoaded _ { editContactsModalShown = false }
handleAction ShowEditTeamModal = modifyLoaded _ { editTeamModalShown = true }
handleAction HideEditTeamModal = modifyLoaded _ { editTeamModalShown = false }
handleAction (ShowEditProfileModal profile) = modifyLoaded _ { editProfileModalShown = Just profile }
handleAction HideEditProfileModal = modifyLoaded _ { editProfileModalShown = Nothing }
handleAction (ShowDeleteProfileModal profile) = modifyLoaded _ { deleteProfileModalShown = Just profile }
handleAction HideDeleteProfileModal = modifyLoaded _ { deleteProfileModalShown = Nothing }

component :: ∀ query output left.
    H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

team :: ∀ query children left.
    Input -> HH.ComponentHTML query (team :: Slot | children) (Async left)
team handle = HH.slot (Proxy :: _ "team") unit component handle absurd
