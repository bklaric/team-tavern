module TeamTavern.Client.Game.Header
    (PlayerStatus(..), State(..), Output, Slot, gameHeader) where

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
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Game.CreateProfile (createProfile)
import TeamTavern.Client.Game.CreateProfile as CreateProfile
import TeamTavern.Client.Game.Edit (editGame)
import TeamTavern.Client.Game.Edit as Edit
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Server.Game.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Input =
    { game :: View.OkContent
    , playerStatus :: PlayerStatus
    }

data Action
    = Receive Input
    | ToggleFilterProfilesSection MouseEvent
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

type State =
    { game :: View.OkContent
    , playerStatus :: PlayerStatus
    , profileFilterVisible :: Boolean
    }

data Output = ToggleFilterProfiles

type Slot = H.Slot (Const Void) Output

type ChildSlots =
    ( editGame :: Edit.Slot Unit
    , createProfile :: CreateProfile.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { game: game @ { title, handle, description, hasProfile }, playerStatus, profileFilterVisible } = HH.div_
    [ HH.h2 [ HP.class_ $ ClassName "card-header"] $ join
        [ pure $ HH.text title
        , guard (isAdmin playerStatus)
            [ HH.button [ HE.onClick $ Just <<< ShowEditGameModal { title, handle, description } ]
                [ HH.i [ HP.class_ $ H.ClassName "fas fa-edit" ] []
                , HH.text "Edit game"
                ]
            ]
        , pure $ divider
        , pure $ HH.span [ HP.class_ $ ClassName "card-subheader" ] [ HH.text "Profiles" ]
        , pure $ HH.button
            [ HE.onClick $ Just <<< ToggleFilterProfilesSection
            , HP.class_ $ H.ClassName if profileFilterVisible then "secondary" else ""
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fas fa-filter" ] []
            , HH.text "Filter profiles"
            ]
        , guard (not hasProfile && isSignedIn playerStatus)
            [ HH.button [ HE.onClick $ Just <<< ShowCreateProfileModal game ]
                [ HH.i [ HP.class_ $ H.ClassName "fas fa-user-plus" ] []
                , HH.text "Create profile"
                ]
            ]
        ]
    , HH.div_ [ editGame $ Just <<< HandleEditGameMessage ]
    , HH.div_ [ createProfile $ Just <<< HandleCreateProfileMessage ]
    ]

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive { game, playerStatus}) =
    H.put { game, playerStatus, profileFilterVisible: false }
handleAction (ToggleFilterProfilesSection event) = do
    H.liftEffect $ preventDefault $ toEvent event
    H.modify_ \state ->
        state { profileFilterVisible = not state.profileFilterVisible }
    H.raise ToggleFilterProfiles
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

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ game, playerStatus } ->
        { game, playerStatus, profileFilterVisible: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

gameHeader
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (gameHeader :: Slot Unit | children) (Async left)
gameHeader input handleOutput =
    HH.slot (SProxy :: SProxy "gameHeader") unit component input handleOutput
