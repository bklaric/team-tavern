module TeamTavern.Client.Pages.Player where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Popover (popoverBody, popoverButtonCaret, popoverContainer, subscribeToWindowClick)
import TeamTavern.Client.Pages.Player.ChangeNickname as ChangeNickname
import TeamTavern.Client.Pages.Player.Details (details)
import TeamTavern.Client.Pages.Player.Details as Details
import TeamTavern.Client.Pages.Player.EditSettings as EditSettings
import TeamTavern.Client.Pages.Player.Profiles (profiles)
import TeamTavern.Client.Pages.Player.Profiles as PlayerProfiles
import TeamTavern.Client.Pages.Player.Teams (teams)
import TeamTavern.Client.Pages.Player.Teams as Teams
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (hardNavigate, navigateWithEvent_)
import TeamTavern.Server.Player.View.SendResponse as View
import Web.Event.Event as E
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

data Action
    = Init String
    | Navigate String MouseEvent
    | ToggleEditAccountPopover MouseEvent
    | CloseEditAccountPopover
    | ShowChangeNicknameModal MouseEvent
    | ShowEditSettingsModal MouseEvent
    | HandleChangeNicknameMessage (Modal.Output ChangeNickname.Message)
    | HandleEditSettingsMessage

data State
    = Empty
    | Player
        { player :: View.OkContent
        , status :: Status
        , editPopoverShown :: Boolean
        , windowSubscription :: H.SubscriptionId
        }
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( details :: Details.Slot
    , profiles :: PlayerProfiles.Slot
    , teams :: Teams.Slot
    , changeNickname :: ChangeNickname.Slot
    , editSettings :: EditSettings.Slot
    )

renderEditAccountButton :: forall slots.
    Boolean -> HH.HTML slots Action
renderEditAccountButton editPopoverShown =
    popoverContainer $
    [ HH.button
        [ HP.class_ $ HH.ClassName "popover-button"
        , HE.onClick $ Just <<< ToggleEditAccountPopover
        ] $
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text "Edit account"
        , popoverButtonCaret editPopoverShown
        ]
    ]
    <>
    popoverBody editPopoverShown
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

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Player { player: { nickname, about }, status, editPopoverShown }) =
    HH.div_
    [ HH.div [ HP.class_ $ ClassName "content-title" ]
        [ HH.div [ HP.class_ $ HH.ClassName "content-title-left"]
            [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ]
                [ HH.text nickname ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "content-title-right" ]
            case status of
            SignedInSelf -> [ renderEditAccountButton editPopoverShown ]
            SignedInOther ->
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
    -- , HH.div_ [ changeNickname $ Just <<< HandleChangeNicknameMessage ]
    -- , HH.div_ [ editSettings $ const $ Just HandleEditSettingsMessage ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
        [ HH.text
            case status of
            SignedInSelf -> "View and edit all your player and team profiles."
            _ -> "View all player and team profiles of player " <> nickname <> "."
        ]
    , details nickname status (SProxy :: SProxy "details")
    , profiles nickname status
    , teams { nickname, status }
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
    playerInfo <- getPlayerInfo
    case playerInfo of
        Just { nickname: nickname' } | content.nickname == nickname' ->
            pure $ Player { player: content, status: SignedInSelf, editPopoverShown: false, windowSubscription }
        Just { nickname: nickname' } ->
            pure $ Player { player: content, status: SignedInOther, editPopoverShown: false, windowSubscription }
        Nothing -> pure $ Player { player: content, status: SignedOut, editPopoverShown: false, windowSubscription }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init nickname') = do
    windowSubscription <- subscribeToWindowClick CloseEditAccountPopover
    state <- H.lift $ loadPlayer nickname' windowSubscription
    H.put state
    let metaNickname =
            case state of
            Player { player: { nickname } } -> nickname
            _ -> nickname'
    H.lift $ Async.fromEffect do
        setMetaTitle $ metaNickname <> " | TeamTavern"
        setMetaDescription $ "View profiles by player " <> metaNickname <> " on TeamTavern."
        setMetaUrl
handleAction (Navigate path mouseEvent) = do
    navigateWithEvent_ path mouseEvent
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
    -- Modal.show (SProxy :: SProxy "changeNickname")
handleAction (ShowEditSettingsModal mouseEvent) = do
    H.liftEffect $ E.preventDefault $ ME.toEvent mouseEvent
    -- Modal.show (SProxy :: SProxy "editSettings")
handleAction (HandleChangeNicknameMessage output) = do
    -- Modal.hide (SProxy :: SProxy "changeNickname")
    case output of
        Modal.OutputRaised (ChangeNickname.NicknameChanged nickname) ->
            hardNavigate $ "/players/" <> nickname
        _ -> pure unit
handleAction HandleEditSettingsMessage =
    -- Modal.hide (SProxy :: SProxy "editSettings")
    pure unit

component :: forall query output left.
    String -> H.Component HH.HTML query String output (Async left)
component nickname = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init nickname
        , receive = Just <<< Init
        }
    }

player
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (player :: Slot Unit | children) (Async left)
player nickname = HH.slot
    (SProxy :: SProxy "player") unit (component nickname) nickname absurd
