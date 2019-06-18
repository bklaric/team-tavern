module TeamTavern.Client.Game.Header
    (PlayerStatus(..), State(..), Slot, gameHeader) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Game.CreateProfile (createProfile)
import TeamTavern.Client.Game.CreateProfile as CreateProfile
import TeamTavern.Client.Game.Edit (editGame)
import TeamTavern.Client.Game.Edit as Edit
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Server.Game.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Receive State
    | ShowEditGameModal Edit.Input MouseEvent
    | ShowCreateProfileModal View.OkContent MouseEvent
    | HandleEditGameMessage (Modal.Message Edit.Message)
    | HandleCreateProfileMessage (Modal.Message CreateProfile.Message)

data PlayerStatus = SignedOut | Player | Administrator

isSignedIn :: PlayerStatus -> Boolean
isSignedIn = case _ of
    SignedOut -> false
    _ -> true

isAdmin :: PlayerStatus -> Boolean
isAdmin = case _ of
    Administrator -> true
    _ -> false

data State = State View.OkContent PlayerStatus

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( editGame :: Edit.Slot Unit
    , createProfile :: CreateProfile.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (State game @ { title, handle, description, hasProfile } playerStatus) = HH.div_
    [ HH.h2 [ HP.class_ $ ClassName "card-header"] [ HH.text title ]
    , HH.div [ HP.class_ $ ClassName "card"] $ join
        [ guard (not hasProfile && isSignedIn playerStatus)
            [ HH.p_ [ HH.a
                [ HP.href ""
                , HE.onClick $ Just <<< ShowCreateProfileModal game
                ]
                [ HH.text "Create profile" ]
            ] ]
        , guard (isAdmin playerStatus)
            [ HH.p_ [ HH.a
                [ HP.href ""
                , HE.onClick
                    $ Just <<< ShowEditGameModal { title, handle, description }
                ]
                [ HH.text "Edit game" ]
            ] ]
        , description <#> \paragraph -> HH.p_ [ HH.text paragraph ]
        ]
    , HH.div_ [ editGame $ Just <<< HandleEditGameMessage ]
    , HH.div_ [ createProfile $ Just <<< HandleCreateProfileMessage ]
    ]

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Receive state) =
    H.put state
handleAction (ShowEditGameModal input event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith input (SProxy :: SProxy "editGame")
handleAction (ShowCreateProfileModal game event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith game (SProxy :: SProxy "createProfile")
handleAction (HandleEditGameMessage message) = do
    Modal.hide (SProxy :: SProxy "editGame")
    case message of
        Modal.Inner (Edit.GameUpdated handle) ->
            H.liftEffect $ navigate_ $ "/games/" <> trim handle
        _ -> pure unit
handleAction (HandleCreateProfileMessage message) = do
    Modal.hide (SProxy :: SProxy "createProfile")
    case message of
        Modal.Inner (CreateProfile.ProfileCreated handle) ->
            H.liftEffect $ navigate_ $ "/games/" <> trim handle
        _ -> pure unit

component :: forall query output left.
    H.Component HH.HTML query State output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

gameHeader
    :: forall query children left
    .  State
    -> HH.ComponentHTML query (gameHeader :: Slot Unit | children) (Async left)
gameHeader state =
    HH.slot (SProxy :: SProxy "gameHeader") unit component state absurd
