module TeamTavern.Client.Pages.Account (Slot, ChildSlots, account) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.ProfilesByPlayer (profilesByPlayer)
import TeamTavern.Client.Components.ProfilesByPlayer as ProfilesByPlayer
import TeamTavern.Client.EditPlayer (editPlayer)
import TeamTavern.Client.EditPlayer as EditPlayer
import TeamTavern.Client.EditProfile (ProfileIlk(..))
import TeamTavern.Client.Script.Cookie (getPlayerId, getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Server.Player.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Nickname = String

data Action
    = Init
    | ShowEditPlayerModal EditPlayer.Input MouseEvent
    | HandleEditPlayerMessage (Modal.Message EditPlayer.Message)

data State
    = Empty
    | Player Nickname
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( editPlayer :: EditPlayer.Slot Unit
    , profilesByPlayer :: ProfilesByPlayer.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Player nickname) = HH.div_
    [ HH.h1 [ HP.class_ $ ClassName "content-title"]
        [ HH.text nickname
        , HH.button
            [ HP.class_ $ HH.ClassName "regular-button title-button"
            , HE.onClick $ Just <<< ShowEditPlayerModal { nickname, about: [] }
            ]
            [ HH.i [ HP.class_ $ H.ClassName "fas fa-edit button-icon" ] []
            , HH.text "Edit account"
            ]
        ]
    , profilesByPlayer nickname Players
    , profilesByPlayer nickname Teams
    , HH.div_ [ editPlayer $ Just <<< HandleEditPlayerMessage ]
    ]
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    playerInfo <- H.liftEffect getPlayerInfo
    case playerInfo of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just { id, nickname } -> do
            H.put $ Player nickname
            H.liftEffect do
                setMetaTitle "Account | TeamTavern"
                setMetaDescription $ "View your account on TeamTavern."
                setMetaUrl
handleAction (ShowEditPlayerModal input event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith input (SProxy :: SProxy "editPlayer")
handleAction (HandleEditPlayerMessage message) = do
    state <- H.get
    Modal.hide (SProxy :: SProxy "editPlayer")
    case message of
        Modal.Inner (EditPlayer.PlayerUpdated nickname) ->
            H.liftEffect $ navigate_ $ "/players/" <> trim nickname
        _ -> pure unit

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init
        , receive = const $ Just Init
        }
    }

account
    :: forall query children left
    . HH.ComponentHTML query (account :: Slot | children) (Async left)
account = HH.slot (SProxy :: SProxy "account") unit component unit absurd
