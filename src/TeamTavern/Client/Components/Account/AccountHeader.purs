module TeamTavern.Client.Components.Account.AccountHeader
    (Nickname, Tab(..), Slot, accountHeader) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Account.EditAccount (editAccount)
import TeamTavern.Client.Components.Account.EditAccount as EditAccount
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Navigate (navigate_)
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Nickname = String

data Tab = Profiles | Conversations | Conversation Nickname

data Input = Input Nickname Tab

type State = Input

type Path = String

data Action
    = Navigate Path MouseEvent
    | Receive Input
    | ShowEditAccountModal MouseEvent
    | HandleEditAccountMessage (Modal.Message EditAccount.Message)

type ChildSlots = (editAccount :: EditAccount.Slot)

type Slot = H.Slot (Const Void) Void Unit

profilesPath :: Path
profilesPath = "/account/profiles"

conversationsPath :: Path
conversationsPath = "/account/conversations"

renderTabs :: forall slots. Tab -> Array (HH.HTML slots Action)
renderTabs Profiles =
    [ HH.span [ HP.class_ $ HH.ClassName "content-title-tab" ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-address-card button-icon" ] []
        , HH.text "View profiles"
        ]
    , HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href conversationsPath
        , HE.onClick $ Just <<< Navigate conversationsPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
        , HH.text "View conversations"
        ]
    ]
renderTabs Conversations =
    [ HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href profilesPath
        , HE.onClick $ Just <<< Navigate profilesPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-address-card button-icon" ] []
        , HH.text "View profiles"
        ]
    , HH.span [ HP.class_ $ HH.ClassName "content-title-tab" ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
        , HH.text "View conversations"
        ]
    ]
renderTabs (Conversation _) =
    [ HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href profilesPath
        , HE.onClick $ Just <<< Navigate profilesPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-address-card button-icon" ] []
        , HH.text "View profiles"
        ]
    , HH.a
        [ HP.class_ $ HH.ClassName "content-title-tab"
        , HP.href conversationsPath
        , HE.onClick $ Just <<< Navigate conversationsPath
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
        , HH.text "View conversations"
        ]
    ]

render :: forall left. Input -> H.ComponentHTML Action ChildSlots (Async left)
render (Input nickname tab) =
    HH.div [ HP.class_ $ HH.ClassName "content-title" ]
        [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ]
            [ HH.text nickname ]
        , HH.div [ HP.class_ $ HH.ClassName "content-title-tabs" ]
            (renderTabs tab)
        , HH.button
            [ HP.class_ $ HH.ClassName "regular-button title-button"
            , HE.onClick $ Just <<< ShowEditAccountModal
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fas fa-edit button-icon" ] []
            , HH.text "Edit account"
            ]
        , HH.div_ [ editAccount $ Just <<< HandleEditAccountMessage ]
        ]

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Navigate path mouseEvent) = do
    liftEffect $ preventDefault $ toEvent mouseEvent
    liftEffect $ navigate_ path
handleAction (Receive input) =
    H.put input
handleAction (ShowEditAccountModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "editAccount")
handleAction (HandleEditAccountMessage message) = do
    state <- H.get
    Modal.hide (SProxy :: SProxy "editAccount")
    case message of
        Modal.Inner (EditAccount.AccountUpdated nickname) ->
            window >>= location >>= reload # H.liftEffect
        _ -> pure unit

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

accountHeader :: forall query children left.
    Nickname -> Tab -> HH.ComponentHTML query (accountHeader :: Slot | children) (Async left)
accountHeader nickname tab = HH.slot (SProxy :: SProxy "accountHeader") unit component (Input nickname tab) absurd
