module TeamTavern.Client.Player (Slot, player) where

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
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Server.Player.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Init String
    | ShowEditPlayerModal EditPlayer.Input MouseEvent
    | HandleEditPlayerMessage (Modal.Message EditPlayer.Message)

data State
    = Empty
    | Player View.OkContent Boolean
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( editPlayer :: EditPlayer.Slot Unit
    , profilesByPlayer :: ProfilesByPlayer.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Player { nickname, about } isCurrentUser) = HH.div_
    [ HH.h2 [ HP.class_ $ ClassName "content-title"] $ join
        [ pure $ HH.text nickname
        , guard isCurrentUser $ pure $
            HH.button
                [ HP.class_ $ HH.ClassName "button-regular title-button"
                , HE.onClick $ Just <<< ShowEditPlayerModal { nickname, about }
                ]
                [ HH.i [ HP.class_ $ H.ClassName "fas fa-edit button-icon" ] []
                , HH.text "Edit account"
                ]
        ]
    , profilesByPlayer nickname
    , HH.div_ [ editPlayer $ Just <<< HandleEditPlayerMessage ]
    ]
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> Async left State
loadPlayer nickname = Async.unify do
    response <- Fetch.fetch_ ("/api/players/by-nickname/" <> nickname)
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerId <- Async.fromEffect getPlayerId
    pure $ Player content (maybe false (_ == content.id) playerId)

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init nickname) = do
    state <- H.lift $ loadPlayer nickname
    H.put state
    pure unit
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
