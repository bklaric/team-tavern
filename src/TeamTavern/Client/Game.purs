module TeamTavern.Client.Game (Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.CreateProfile (createProfile)
import TeamTavern.Client.CreateProfile as CreateProfile
import TeamTavern.Client.EditGame (editGame)
import TeamTavern.Client.EditGame as EditGame
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Game.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Init String
    | ShowEditGameModal MouseEvent
    | ShowCreateProfileModal MouseEvent
    | HandleEditGameMessage (Modal.Message EditGame.Message)
    | HandleCreateProfileMessage (Modal.Message CreateProfile.Message)

data PlayerStatus = SignedOut | Player | Administrator

createPlayerStatus :: Maybe Int -> Int -> PlayerStatus
createPlayerStatus playerId administratorId =
    case playerId of
    Just playerId' ->
        if playerId' == administratorId
        then Administrator
        else Player
    _ -> SignedOut

isSignedIn :: PlayerStatus -> Boolean
isSignedIn = case _ of
    SignedOut -> false
    _ -> true

isAdmin :: PlayerStatus -> Boolean
isAdmin = case _ of
    Administrator -> true
    _ -> false

data State
    = Empty
    | Game View.OkContent PlayerStatus
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( editGame :: EditGame.Slot Unit
    , createProfile :: CreateProfile.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render
    (Game { title, handle, description, hasProfile } playerStatus) =
    HH.div [ HP.id_ "game"] $ join
    [ pure $ HH.h2_ [ HH.text title ]
    , guard (not hasProfile && isSignedIn playerStatus) $ pure $ HH.p_ [
        HH.a
        [ HP.href ""
        , HE.onClick $ Just <<< ShowCreateProfileModal
        ]
        [ HH.text "Create profile" ] ]
    , guard (isAdmin playerStatus) $ pure $ HH.p_ [
        HH.a
        [ HP.href ""
        , HE.onClick $ Just <<< ShowEditGameModal
        ]
        [ HH.text "Edit game" ] ]
    , pure $ HH.p_ [ HH.text description ]
    , pure $ HH.div_ [ editGame
        { title, handle, description } $ Just <<< HandleEditGameMessage ]
    , pure $ HH.div_ [ createProfile
        handle $ Just <<< HandleCreateProfileMessage ]
   ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> Async left State
loadGame handle = Async.unify do
    response <- Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerId <- Async.fromEffect getPlayerId
    pure $ Game content (createPlayerStatus playerId content.administratorId)

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init handle) = do
    state <- H.lift $ loadGame handle
    H.put state
    pure unit
handleAction (ShowEditGameModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "editGame")
handleAction (ShowCreateProfileModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "createProfile")
handleAction (HandleEditGameMessage message) = do
    Modal.hide (SProxy :: SProxy "editGame")
    case message of
        Modal.Inner (EditGame.GameUpdated handle) ->
            H.liftEffect $ navigate_ $ "/games/" <> trim handle
        _ -> pure unit
handleAction (HandleCreateProfileMessage message) = do
    Modal.hide (SProxy :: SProxy "createProfile")
    case message of
        Modal.Inner (CreateProfile.ProfileCreated handle) ->
            H.liftEffect $ navigate_ $ "/games/" <> trim handle
        _ -> pure unit

component :: forall query output left.
    String -> H.Component HH.HTML query String output (Async left)
component handle = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init handle
        , receive = Just <<< Init
        }
    }

game :: forall query children left.
    String -> HH.ComponentHTML query (game :: Slot Unit | children) (Async left)
game handle =
    HH.slot (SProxy :: SProxy "game") unit (component handle) handle absurd
