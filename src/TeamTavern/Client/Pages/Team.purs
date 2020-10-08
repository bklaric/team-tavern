module TeamTavern.Client.Pages.Team where

import Prelude

import Async (Async)
import Async as Async
import Async.Aff (affToAsync)
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Player.ChangeNickname (changeNickname)
import TeamTavern.Client.Pages.Player.ChangeNickname as ChangeNickname
import TeamTavern.Client.Pages.Player.Details as Details
import TeamTavern.Client.Pages.Player.EditSettings (editSettings)
import TeamTavern.Client.Pages.Player.EditSettings as EditSettings
import TeamTavern.Client.Pages.Player.PlayerProfiles (playerProfiles)
import TeamTavern.Client.Pages.Player.PlayerProfiles as PlayerProfiles
import TeamTavern.Client.Pages.Player.TeamProfiles (teamProfiles)
import TeamTavern.Client.Pages.Player.TeamProfiles as TeamProfiles
import TeamTavern.Client.Pages.Player.Teams (get, teams)
import TeamTavern.Client.Pages.Player.Teams as Teams
import TeamTavern.Client.Pages.Player.Types (PlayerStatus(..))
import TeamTavern.Client.Pages.Team.Details (details)
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Player.View.SendResponse as View
import TeamTavern.Server.Team.View (Team)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

type Input = { handle :: String }

data State
    = Empty Input
    | Team
        { team :: Team }
    | Player
        { player :: View.OkContent
        , status :: PlayerStatus
        , editPopoverShown :: Boolean
        , windowSubscription :: H.SubscriptionId
        }
    | NotFound
    | Error

data Action
    = Initialize
    | Navigate String MouseEvent
    | ToggleEditAccountPopover MouseEvent
    | CloseEditAccountPopover
    | ShowChangeNicknameModal MouseEvent
    | ShowEditSettingsModal MouseEvent
    | HandleChangeNicknameMessage (Modal.Message ChangeNickname.Message)
    | HandleEditSettingsMessage

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( details :: Details.Slot
    , playerProfiles :: PlayerProfiles.Slot
    , teams :: Teams.Slot
    , changeNickname :: ChangeNickname.Slot
    , editSettings :: EditSettings.Slot
    , discordServer :: Copyable.Slot
    )

renderEditAccountButton :: forall slots.
    Boolean -> HH.HTML slots Action
renderEditAccountButton editPopoverShown =
    HH.div
    [ HP.class_ $ HH.ClassName "popover-container" ] $
    [ HH.button
        [ HP.class_ $ HH.ClassName "popover-button"
        , HE.onClick $ Just <<< ToggleEditAccountPopover
        ] $
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text "Edit account"
        , HH.i
            [ HP.class_ $ HH.ClassName $ "fas popover-button-caret "
                <> if editPopoverShown then "fa-caret-up" else "fa-caret-down"
            ]
            []
        ]
    ]
    <> (
    if editPopoverShown
    then Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "popover" ]
        [ HH.div
            [ HP.class_ $ HH.ClassName "popover-item"
            , HE.onClick $ Just <<< ShowChangeNicknameModal
            ]
            [ HH.text "Change nickname" ]
        , HH.div
            [ HP.class_ $ HH.ClassName "popover-item"
            , HE.onClick $ Just <<< ShowEditSettingsModal
            ]
            [ HH.text "Edit account settings" ]
        ]
    else [])

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Team { team: team' } ) =
    HH.div_
    [ HH.div [ HP.class_ $ ClassName "content-title" ]
        [ HH.div [ HP.class_ $ HH.ClassName "content-title-left"]
            [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ]
                [ HH.text team'.name ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "content-title-right" ]
            -- case status of
            -- SamePlayer -> [ renderEditAccountButton editPopoverShown ]
            -- SignedIn _ ->
            --     [ HH.div [ HP.class_ $ HH.ClassName "content-title-tabs" ] $
            --         [ HH.a
            --             [ HP.class_ $ HH.ClassName "content-title-tab"
            --             , HP.href $ "/conversations/" <> nickname
            --             , HE.onClick $ Just <<< Navigate ("/conversations/" <> nickname)
            --             ]
            --             [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
            --             , HH.text "Message player"
            --             ]
            --         ]
            --     ]
            -- _ -> []
            []
        ]
    -- , HH.div_ [ changeNickname $ Just <<< HandleChangeNicknameMessage ]
    -- , HH.div_ [ editSettings $ const $ Just HandleEditSettingsMessage ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
        [ HH.text
            -- case status of
            -- SamePlayer -> "View and edit all your player and team profiles."
            -- _ -> "View all player and team profiles of player " <> nickname <> "."
            "This is a team, lmao!"
        ]
    , details team'
    -- , playerProfiles nickname status (SProxy :: SProxy "playerProfiles")
    -- , teams { nickname, status } -- (SProxy :: SProxy "teamProfiles")
    ]
render (Player { player: { nickname, about }, status, editPopoverShown }) =
    HH.div_
    [ HH.div [ HP.class_ $ ClassName "content-title" ]
        [ HH.div [ HP.class_ $ HH.ClassName "content-title-left"]
            [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ]
                [ HH.text nickname ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "content-title-right" ]
            case status of
            SamePlayer -> [ renderEditAccountButton editPopoverShown ]
            SignedIn _ ->
                [ HH.div [ HP.class_ $ HH.ClassName "content-title-tabs" ] $
                    [ HH.a
                        [ HP.class_ $ HH.ClassName "content-title-tab"
                        , HP.href $ "/conversations/" <> nickname
                        , HE.onClick $ Just <<< Navigate ("/conversations/" <> nickname)
                        ]
                        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
                        , HH.text "Message player"
                        ]
                    ]
                ]
            _ -> []
        ]
    , HH.div_ [ changeNickname $ Just <<< HandleChangeNicknameMessage ]
    , HH.div_ [ editSettings $ const $ Just HandleEditSettingsMessage ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
        [ HH.text
            case status of
            SamePlayer -> "View and edit all your player and team profiles."
            _ -> "View all player and team profiles of player " <> nickname <> "."
        ]
    -- , details nickname status (SProxy :: SProxy "details")
    , playerProfiles nickname status (SProxy :: SProxy "playerProfiles")
    , teams { nickname, status } -- (SProxy :: SProxy "teamProfiles")
    ]
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> H.SubscriptionId -> Async left State
loadPlayer nickname windowSubscription = Async.unify do
    response <- Fetch.fetch_ ("/api/players/by-nickname/" <> nickname)
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerInfo <- Async.fromEffect getPlayerInfo
    case playerInfo of
        Just { nickname: nickname' } | content.nickname == nickname' ->
            pure $ Player { player: content, status: SamePlayer, editPopoverShown: false, windowSubscription }
        Just { nickname: nickname' } ->
            pure $ Player { player: content, status: SignedIn nickname', editPopoverShown: false, windowSubscription }
        Nothing -> pure $ Player { player: content, status: SignedOut, editPopoverShown: false, windowSubscription }

loadTeam handle timezone =
    get $ "/api/teams/by-handle/" <> handle <> "?timezone=" <> timezone

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    -- window <- H.liftEffect $ Window.toEventTarget <$> window
    -- let windowEventSource = ES.eventListenerEventSource
    --         (E.EventType "click") window \_ -> Just CloseEditAccountPopover
    -- windowSubscription <- H.subscribe $ ES.hoist affToAsync windowEventSource
    -- state <- H.lift $ loadPlayer nickname' windowSubscription
    -- H.put state
    -- let metaNickname =
    --         case state of
    --         Player { player: { nickname } } -> nickname
    --         _ -> nickname'
    state <- H.get
    case state of
        Empty { handle } -> do
            timezone <- H.liftEffect getClientTimezone
            team' <- H.lift $ loadTeam handle timezone
            case team' of
                Just team'' -> H.put $ Team
                    { team: team'' }
                _ -> pure unit
        _ -> pure unit
    H.lift $ Async.fromEffect do
        setMetaTitle $ "aoeu" <> " | TeamTavern"
        setMetaDescription $ "View profiles by player " <> "aoeueuue" <> " on TeamTavern."
        setMetaUrl
handleAction (Navigate path mouseEvent) = do
    H.liftEffect $ navigateWithEvent_ path mouseEvent
handleAction (ToggleEditAccountPopover mouseEvent) = do
    H.liftEffect $ E.preventDefault $ ME.toEvent mouseEvent
    H.liftEffect $ E.stopPropagation $ ME.toEvent mouseEvent
    H.modify_ case _ of
        Player state -> Player state { editPopoverShown = not state.editPopoverShown }
        state -> state
handleAction (CloseEditAccountPopover) =
    H.modify_ case _ of
        Player state -> Player state { editPopoverShown = false }
        state -> state
handleAction (ShowChangeNicknameModal mouseEvent) = do
    H.liftEffect $ E.preventDefault $ ME.toEvent mouseEvent
    Modal.show (SProxy :: SProxy "changeNickname")
handleAction (ShowEditSettingsModal mouseEvent) = do
    H.liftEffect $ E.preventDefault $ ME.toEvent mouseEvent
    Modal.show (SProxy :: SProxy "editSettings")
handleAction (HandleChangeNicknameMessage message) = do
    Modal.hide (SProxy :: SProxy "changeNickname")
    case message of
        Modal.Inner (ChangeNickname.NicknameChanged nickname) ->
            window >>= location >>= setHref ("/players/" <> nickname) # H.liftEffect
        _ -> pure unit
handleAction HandleEditSettingsMessage = do
    Modal.hide (SProxy :: SProxy "editSettings")

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
