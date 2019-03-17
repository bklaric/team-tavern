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
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.EditGame (editGame)
import TeamTavern.Client.EditGame as EditGame
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Game.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Init String
    | ShowEditModal MouseEvent
    | HandleModalMessage (Modal.Message EditGame.Message)

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

data EditModalStatus = Shown | Hidden

data State
    = Empty
    | Game View.OkContent PlayerStatus EditModalStatus
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( profiles :: Anchor.Slot Int
    , createProfile :: Anchor.Slot Unit
    , editGame :: EditGame.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render
    (Game { title, handle, description, hasProfile } playerStatus modalStatus) =
    HH.div [ HP.id_ "game"] $ join
    [ pure $ HH.h2_ [ HH.text title ]
    , guard (not hasProfile && isSignedIn playerStatus) $ pure $ HH.p_ [
        navigationAnchor (SProxy :: SProxy "createProfile")
        { path: "/games/" <> handle <> "/profiles/create"
        , text: "Create profile"
        } ]
    , guard (isAdmin playerStatus) $ pure $ HH.p_ [
        HH.a
        [ HP.href ""
        , HE.onClick $ Just <<< ShowEditModal
        ]
        [ HH.text "Edit game" ] ]
    , pure $ HH.p_ [ HH.text description ]
    , pure $ HH.div_ case modalStatus of
        Shown -> [ editGame
            { title, handle, description } $ Just <<< HandleModalMessage ]
        _ -> []
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
    pure $ Game
        content (createPlayerStatus playerId content.administratorId) Hidden

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init handle) = do
    state <- H.lift $ loadGame handle
    H.put state
    pure unit
handleAction (ShowEditModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    H.modify_ $
        case _ of
        Game content playerStatus modalStatus -> Game content playerStatus Shown
        other -> other
handleAction (HandleModalMessage message) = do
    state <- H.get
    case state, message of
        Game content player modal, Modal.CloseModal ->
            H.put $ Game content player Hidden
        Game content player modal, Modal.Inner (EditGame.GameUpdated handle) -> do
            H.liftEffect $ navigate_ $ "/games/" <> trim handle
        _, _ -> pure unit

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
