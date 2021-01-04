module TeamTavern.Client.Components.Boarding.GameInput (Input, Output, Slot, gameInput) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.RadioCard (radioCard, radioCards)
import TeamTavern.Routes.ViewAllGames as ViewAllGames
import TeamTavern.Routes.ViewGame as ViewGame

type Input = Maybe ViewGame.OkContent

type Output = ViewGame.OkContent

type State =
    { games :: ViewAllGames.OkContent
    , selected :: Maybe ViewGame.OkContent
    }

data Action = Initialize | SelectGame ViewAllGames.OkGameContent

type Slot = H.Slot (Const Void) Output Unit

render :: forall slots. State -> HH.HTML slots Action
render { games, selected } =
    HH.div_
    [ radioCards $
        ( games <#> \game ->
            radioCard
            ("/images/" <> game.handle <> "-tile.jpg")
            (maybe false (_.handle >>> (_ == game.handle)) selected)
            (SelectGame game)
            [ HH.img
                [ HP.class_ $ HH.ClassName "top-bar-game-icon"
                , HP.src $ "/images/" <> game.handle <> "-icon.png"
                ]
            , HH.text game.title
            ]
        )
    ]

loadGames :: forall left. Async left (Maybe ViewAllGames.OkContent)
loadGames = Async.unify do
    response <- Fetch.fetch_ "/api/games" # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

loadGame :: forall left. String -> Async left (Maybe ViewGame.OkContent)
loadGame handle = Async.unify do
    response <- Fetch.fetch_ ("/api/games/by-handle/" <> handle) # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

handleAction :: forall action slots left.
    Action -> H.HalogenM State action slots Output (Async left) Unit
handleAction Initialize = do
    games <- H.lift $ maybe [] identity <$> loadGames
    H.modify_ _ { games = games }
handleAction (SelectGame game) = do
    { selected } <- H.get
    case selected of
        Just selected' | selected'.handle == game.handle -> pure unit
        _ -> do
            game' <- H.lift $ loadGame game.handle
            case game' of
                Just game'' -> do
                    H.modify_ _ { selected = Just game'' }
                    H.raise game''
                Nothing -> pure unit

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: { selected: _, games: [] }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

gameInput
    :: forall action slots left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (gameInput :: Slot | slots) (Async left)
gameInput input handleOutput =
    HH.slot (SProxy :: SProxy "gameInput") unit component input handleOutput
