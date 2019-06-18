module TeamTavern.Client.Game (Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Game.Header (gameHeader)
import TeamTavern.Client.Game.Header as Header
import TeamTavern.Client.Game.Profiles (gameProfiles)
import TeamTavern.Client.Game.Profiles as Profiles
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Server.Game.View.SendResponse as View

data Action = Init String

createPlayerStatus :: Maybe Int -> Int -> Header.PlayerStatus
createPlayerStatus playerId administratorId =
    case playerId of
    Just playerId' ->
        if playerId' == administratorId
        then Header.Administrator
        else Header.Player
    _ -> Header.SignedOut

data State
    = Empty
    | Game View.OkContent Header.PlayerStatus
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( gameHeader :: Header.Slot Unit
    , gameProfiles :: Profiles.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render
    (Game game' playerStatus) = HH.div_
    [ gameHeader $ Header.State game' playerStatus
    , gameProfiles game'
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
