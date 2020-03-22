module TeamTavern.Client.Components.Account.AccountHeader
    (Nickname, Tab(..), Slot, accountHeader) where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import TeamTavern.Client.Components.Account.ChangeNickname (changeNickname)
import TeamTavern.Client.Components.Account.ChangeNickname as ChangeNickname
import TeamTavern.Client.Components.Account.EditAccount (editAccount)
import TeamTavern.Client.Components.Account.EditAccount as EditAccount
import TeamTavern.Client.Components.Account.EditSettings (editSettings)
import TeamTavern.Client.Components.Account.EditSettings as EditSettings
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Navigate (navigate_)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Nickname = String

type EditPopoverShown = Boolean

data Tab = Profiles | Conversations | Conversation Nickname

type Input =
    { nickname :: Nickname
    , tab :: Tab
    }

type State =
    { nickname :: Nickname
    , tab :: Tab
    , editPopoverShown :: EditPopoverShown
    , windowSubscription :: Maybe (H.SubscriptionId)
    }

type Path = String

data Action
    = Init
    | Finalize
    | Navigate Path MouseEvent
    | Receive Input
    | ToggleEditAccountPopover MouseEvent
    | CloseEditAccountPopover
    | ShowChangeNicknameModal MouseEvent
    | ShowEditSettingsModal MouseEvent
    | HandleChangeNicknameMessage (Modal.Message ChangeNickname.Message)
    | HandleEditSettingsMessage (Modal.Message EditSettings.Message)

type ChildSlots =
    ( editAccount :: EditAccount.Slot
    , changeNickname :: ChangeNickname.Slot
    , editSettings :: EditSettings.Slot
    )

type Slot = H.Slot (Const Void) Void Unit

profilesPath :: Path
profilesPath = "/account/profiles"

conversationsPath :: Path
conversationsPath = "/account/conversations"

renderTabs :: forall slots. Tab -> Array (HH.HTML slots Action)
renderTabs Profiles =
    [ HH.span [ HP.class_ $ HH.ClassName "content-title-tab" ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-address-card button-icon" ] []
        , HH.text "Profiles"
        ]
    , HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href conversationsPath
        , HE.onClick $ Just <<< Navigate conversationsPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
        , HH.text "Conversations"
        ]
    ]
renderTabs Conversations =
    [ HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href profilesPath
        , HE.onClick $ Just <<< Navigate profilesPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-address-card button-icon" ] []
        , HH.text "Profiles"
        ]
    , HH.span [ HP.class_ $ HH.ClassName "content-title-tab" ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
        , HH.text "Conversations"
        ]
    ]
renderTabs (Conversation _) =
    [ HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href profilesPath
        , HE.onClick $ Just <<< Navigate profilesPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-address-card button-icon" ] []
        , HH.text "Profiles"
        ]
    , HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href conversationsPath
        , HE.onClick $ Just <<< Navigate conversationsPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
        , HH.text "Conversations"
        ]
    ]

renderEditAccountButton :: forall slots.
    EditPopoverShown -> HH.HTML slots Action
renderEditAccountButton editPopoverShown =
    HH.button
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
    <> (Array.catMaybes $ Array.singleton
    if editPopoverShown
    then Just $
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
        , HH.div [ HP.class_ $ HH.ClassName "popover-item" ]
            [ HH.text "Edit player details" ]
        ]
    else Nothing)


render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { nickname, tab, editPopoverShown } = HH.div_ $
    [ HH.div [ HP.class_ $ HH.ClassName "content-title" ]
        [ HH.div [ HP.class_ $ HH.ClassName "content-title-left" ]
            [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ]
                [ HH.text nickname ]
            , HH.div [ HP.class_ $ HH.ClassName "content-title-tabs" ]
                (renderTabs tab)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "content-title-right" ]
            [ renderEditAccountButton editPopoverShown ]
        ]
    ]
    -- <> [ HH.div_ [ editAccount $ Just <<< HandleChangeNicknameMessage ] ]
    <> [ HH.div_ [ changeNickname $ Just <<< HandleChangeNicknameMessage ] ]
    <> [ HH.div_ [ editSettings $ Just <<< HandleEditSettingsMessage ] ]
    <>
    case tab of
    Profiles -> pure $
        HH.p [ HP.class_ $ HH.ClassName "content-description" ]
            [ HH.text "View and edit all your player and team profiles." ]
    Conversations -> pure $
        HH.p [ HP.class_ $ HH.ClassName "content-description" ]
            [ HH.text "View all your conversations with other players." ]
    Conversation _ -> []

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListenerEventSource
            (E.EventType "click") window \_ -> Just CloseEditAccountPopover
    windowSubscription <- H.subscribe $ ES.hoist affToAsync windowEventSource
    H.modify_ (_ { windowSubscription = Just windowSubscription })
    pure unit
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction (Navigate path event) = do
    H.liftEffect $ preventDefault $ toEvent event
    H.liftEffect $ navigate_ path
handleAction (Receive { nickname, tab }) = H.put
    { nickname, tab, editPopoverShown: false, windowSubscription: Nothing }
handleAction (ShowChangeNicknameModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "changeNickname")
handleAction (ShowEditSettingsModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "editSettings")
handleAction (ToggleEditAccountPopover event) = do
    H.liftEffect $ preventDefault $ toEvent event
    H.liftEffect $ stopPropagation $ toEvent event
    H.modify_ \state -> state { editPopoverShown = not state.editPopoverShown }
handleAction CloseEditAccountPopover =
    H.modify_ (_ { editPopoverShown = false })
handleAction (HandleChangeNicknameMessage message) = do
    Modal.hide (SProxy :: SProxy "changeNickname")
    state <- H.get
    case message of
        Modal.Inner ChangeNickname.NicknameChanged ->
            window >>= location >>= reload # H.liftEffect
        _ -> pure unit
handleAction (HandleEditSettingsMessage message) = do
    Modal.hide (SProxy :: SProxy "editSettings")

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ nickname, tab } ->
        { nickname, tab, editPopoverShown: false, windowSubscription: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Init
        , finalize = Just Finalize
        }
    }

accountHeader :: forall query children left.
    Nickname -> Tab -> HH.ComponentHTML query (accountHeader :: Slot | children) (Async left)
accountHeader nickname tab = HH.slot (SProxy :: SProxy "accountHeader") unit component { nickname, tab } absurd
