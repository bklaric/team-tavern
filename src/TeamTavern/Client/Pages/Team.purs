module TeamTavern.Client.Pages.Team (Input, Slot, team) where

import Prelude

import Async (Async)
import Async as Async
import CSS as CSS
import Client.Components.Copyable as Copyable
import Control.Monad.State (class MonadState)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (foldl)
import Data.List as List
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.MultiMap as MultiMap
import Data.NonEmpty (NonEmpty(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import TeamTavern.Client.Components.Button (regularIconButton)
import TeamTavern.Client.Components.Content (contentDescription, contentHeader, contentHeading)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Pages.Team.Details (details)
import TeamTavern.Client.Pages.Team.EditProfile (editProfile)
import TeamTavern.Client.Pages.Team.EditProfile as EditProfile
import TeamTavern.Client.Pages.Team.EditTeam (editTeam)
import TeamTavern.Client.Pages.Team.EditTeam as EditTeam
import TeamTavern.Client.Pages.Team.Profiles (profiles)
import TeamTavern.Client.Pages.Team.Status (Status(..), getStatus)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Team.View (Team, Profile)

type Input = { handle :: String }

type Loaded =
    { team :: Team
    , status :: Status
    , showEditTeamModal :: Boolean
    , showEditProfileModal :: Maybe Profile
    }

data State
    = Empty Input
    | Loaded Loaded
    | NotFound
    | Error

data Action
    = Initialize
    | ShowEditTeamModal
    | HideEditTeamModal
    | ShowEditProfileModal Profile
    | HideEditProfileModal

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( messageOwner :: NavigationAnchor.Slot Unit
    , discordServer :: Copyable.Slot
    , games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    , editTeam :: EditTeam.Slot
    , editProfile :: EditProfile.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { team: team', status, showEditTeamModal, showEditProfileModal } ) =
    HH.div_  $
    [ contentHeader
        [ HH.div_ [ contentHeading team'.name ]
        , HH.div_
            case status of
            SignedInOwner -> [ regularIconButton "fas fa-edit" "Edit team" ShowEditTeamModal ]
            SignedInOther ->
                [ navigationAnchor (SProxy :: SProxy "messageOwner")
                    { path: "/conversations/" <> team'.owner
                    , content: HH.span [ HC.style $ CSS.fontWeight $ CSS.weight 500.0 ]
                        [ HH.i [ HS.class_ "fas fa-envelope button-icon" ] []
                        , HH.text "Message team owner"
                        ]
                    }
                ]
            SignedOut -> []
        ]
    , contentDescription "This is a team, lmao!"
    , details team'
    , profiles team'.handle team'.profiles ShowEditProfileModal
    ]
    <>
    (if showEditTeamModal
    then [ editTeam team' (const $ Just HideEditTeamModal) ]
    else [])
    <>
    case showEditProfileModal of
    Nothing -> []
    Just profile -> Array.singleton $
        editProfile
        { teamHandle: team'.handle
        , gameHandle: profile.handle
        , title: profile.title
        , fields: profile.fields
        , profile:
            { fieldValues:
                foldl
                (\fieldValues { fieldKey, optionKeys } ->
                    case Array.uncons optionKeys of
                    Nothing -> fieldValues
                    Just { head, tail } ->
                        MultiMap.insertOrReplace fieldKey
                        (NonEmptyList $ NonEmpty head $ List.fromFoldable tail) fieldValues
                )
                MultiMap.empty
                profile.fieldValues
            , newOrReturning: profile.newOrReturning
            , ambitions: intercalate "\n\n" profile.ambitions
            }
        }
        (const $ Just HideEditProfileModal)
render NotFound = HH.p_ [ HH.text "Team could not be found." ]
render Error = HH.p_ [ HH.text "There has been an error loading the team. Please try again later." ]

loadTeam :: forall left. String -> Async left (Maybe Team)
loadTeam handle = do
    timezone <- getClientTimezone
    get $ "/api/teams/by-handle/" <> handle <> "?timezone=" <> timezone

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
        Empty { handle } -> do
            team' <- H.lift $ loadTeam handle
            case team' of
                Just team'' -> do
                    status <- getStatus team''.owner
                    H.put $ Loaded
                        { team: team''
                        , status
                        , showEditTeamModal: false
                        , showEditProfileModal: Nothing
                        }
                _ -> pure unit
        _ -> pure unit
    H.lift $ Async.fromEffect do
        setMetaTitle $ "aoeu" <> " | TeamTavern"
        setMetaDescription $ "View profiles by player " <> "aoeueuue" <> " on TeamTavern."
        setMetaUrl
handleAction ShowEditTeamModal = modifyLoaded _ { showEditTeamModal = true }
handleAction HideEditTeamModal = modifyLoaded _ { showEditTeamModal = false }
handleAction (ShowEditProfileModal profile) = modifyLoaded _ { showEditProfileModal = Just profile }
handleAction HideEditProfileModal = modifyLoaded _ { showEditProfileModal = Nothing }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

team :: forall query children left.
    Input -> HH.ComponentHTML query (team :: Slot | children) (Async left)
team handle = HH.slot (SProxy :: SProxy "team") unit component handle absurd
