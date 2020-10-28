module TeamTavern.Client.Pages.Player (Input, Slot, player) where

import Prelude

import Async (Async)
import Async as Async
import CSS as CSS
import Client.Components.Copyable as Copyable
import Control.Monad.State (class MonadState)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Content (contentHeader, contentHeading)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Popover (popover, popoverButtonCaret, subscribeToWindowClick)
import TeamTavern.Client.Pages.Player.CreateTeam (createTeam)
import TeamTavern.Client.Pages.Player.CreateTeam as CreateTeam
import TeamTavern.Client.Pages.Player.Details (details)
import TeamTavern.Client.Pages.Player.EditDetails (editDetails)
import TeamTavern.Client.Pages.Player.EditDetails as EditDetails
import TeamTavern.Client.Pages.Player.EditProfile (editProfile)
import TeamTavern.Client.Pages.Player.EditProfile as EditProfile
import TeamTavern.Client.Pages.Player.EditSettings as EditSettings
import TeamTavern.Client.Pages.Player.Profiles (profiles)
import TeamTavern.Client.Pages.Player.Status (Status(..), getStatus)
import TeamTavern.Client.Pages.Player.Teams (teams)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.ViewPlayer as ViewPlayer
import Web.Event.Event as E
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

type Input = { nickname :: String }

type Loaded =
    { player :: ViewPlayer.OkContent
    , status :: Status
    , editPopoverShown :: Boolean
    , windowSubscription :: H.SubscriptionId
    , editPlayerModalShown :: Boolean
    , editSettingsModalShown :: Boolean
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
    | ToggleEditAccountPopover MouseEvent
    | CloseEditAccountPopover
    | ShowEditPlayerModal
    | HideEditPlayerModal
    | ShowEditSettingsModal
    | HideEditSettingsModal
    | ShowEditProfileModal ViewPlayer.OkContentProfile
    | HideEditProfileModal
    | ShowCreateTeamModal
    | HideCreateTeamModal

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( discordTag :: Copyable.Slot
    , team :: NavigationAnchor.Slot String
    , editSettings :: EditSettings.Slot
    , messagePlayer :: NavigationAnchor.Slot Unit
    , games :: NavigationAnchor.Slot String
    , editDetails :: EditDetails.Slot
    , createProfile :: H.Slot (Const Void) Void Unit
    , editProfile :: EditProfile.Slot
    , createTeam :: CreateTeam.Slot
    )

renderEditAccountButton :: forall slots. Boolean -> HH.HTML slots Action
renderEditAccountButton editPopoverShown =
    popover
    editPopoverShown
    [ HH.button
        [ HP.class_ $ HH.ClassName "popover-button"
        , HE.onClick $ Just <<< ToggleEditAccountPopover
        ] $
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text "Edit account"
        , popoverButtonCaret editPopoverShown
        ]
    ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "popover-item"
        , HE.onClick $ const $ Just ShowEditPlayerModal
        ]
        [ HH.text "Edit player" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "popover-item"
        , HE.onClick $ const $ Just ShowEditSettingsModal
        ]
        [ HH.text "Edit account settings" ]
    ]

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded state @ { player: player', status, editPopoverShown }) =
    HH.div_ $
    [ contentHeader
        [ HH.div_ [ contentHeading player'.nickname ]
        , HH.div_
            case status of
            SignedInSelf -> [ renderEditAccountButton editPopoverShown ]
            SignedInOther ->
                [ navigationAnchor (SProxy :: SProxy "messagePlayer")
                    { path: "/conversations/" <> player'.nickname
                    , content: HH.span [ HC.style $ CSS.fontWeight $ CSS.weight 500.0 ]
                        [ HH.i [ HS.class_ "fas fa-envelope button-icon" ] []
                        , HH.text "Message player"
                        ]
                    }
                ]
            SignedOut -> []
        ]
    -- , HH.div_ [ changeNickname $ Just <<< HandleChangeNicknameMessage ]
    -- , HH.div_ [ editSettings $ const $ Just HandleEditSettingsMessage ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
        [ HH.text
            case status of
            SignedInSelf -> "View and edit all your player and team profiles."
            _ -> "View all player and team profiles of player " <> player'.nickname <> "."
        ]
    , details player'
    , profiles player' ShowEditProfileModal
    , teams player' ShowCreateTeamModal status
    ]
    <>
    ( if state.editPlayerModalShown
        then [ editDetails player' $ const $ Just HideEditPlayerModal ]
        else []
    )
    <>
    ( if state.createTeamModalShown
        then [ createTeam $ const $ Just HideCreateTeamModal ]
        else []
    )
    <>
    ( case state.editProfileModalShown of
        Just profile -> [ editProfile { nickname: player'.nickname, profile } $ const $ Just HideEditProfileModal ]
        Nothing -> []
    )
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
        Empty input ->
            handleAction $ Receive input
        _ -> pure unit
handleAction (Receive { nickname }) = do
    windowSubscription <- subscribeToWindowClick CloseEditAccountPopover
    player' <- H.lift $ loadPlayer nickname
    case player' of
        Just player'' -> do
            status <- getStatus player''.nickname
            H.put $ Loaded
                { player: player''
                , status
                , editPopoverShown: false
                , windowSubscription
                , editPlayerModalShown: false
                , editSettingsModalShown: false
                , editProfileModalShown: Nothing
                , createTeamModalShown: false
                }
            H.lift $ Async.fromEffect do
                setMetaTitle $ "aoeuaoeu" <> " | TeamTavern"
                setMetaDescription $ "View profiles by player " <> "aoeuaoeu" <> " on TeamTavern."
                setMetaUrl
        _ -> pure unit

handleAction (ToggleEditAccountPopover mouseEvent) = do
    H.liftEffect $ E.stopPropagation $ ME.toEvent mouseEvent
    modifyLoaded \state -> state { editPopoverShown = not state.editPopoverShown }
handleAction (CloseEditAccountPopover) = modifyLoaded _ { editPopoverShown = false }
handleAction ShowEditPlayerModal = modifyLoaded _ { editPlayerModalShown = true }
handleAction HideEditPlayerModal = modifyLoaded _ { editPlayerModalShown = false }
handleAction ShowEditSettingsModal = modifyLoaded _ { editSettingsModalShown = true }
handleAction HideEditSettingsModal = modifyLoaded _ { editSettingsModalShown = false }
handleAction (ShowEditProfileModal profile) =
    modifyLoaded _ { editProfileModalShown = Just profile }
handleAction HideEditProfileModal =
    modifyLoaded _ { editProfileModalShown = Nothing }
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
