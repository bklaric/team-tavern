module TeamTavern.Client.Components.Boarding.GameInput (Input, Output, Slot, gameInput) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.RadioCard (radioCard, radioCards)
import TeamTavern.Client.Shared.Slot (Slot_O_)
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import TeamTavern.Routes.Game.ViewGame as ViewGame
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async as Json

type Input = Maybe ViewGame.OkContent

type Output = ViewGame.OkContent

type State =
    { games :: ViewAllGames.OkContent
    , selected :: Maybe ViewGame.OkContent
    }

data Action = Initialize | SelectGame ViewAllGames.OkGameContent

type Slot = Slot_O_ Output

render :: ∀ slots. State -> HH.HTML slots Action
render { games, selected } =
    HH.div_
    [ radioCards $
        ( games <#> \game ->
            radioCard
            ("/images/" <> game.handle <> "/tile")
            (maybe false (_.handle >>> (_ == game.handle)) selected)
            (SelectGame game)
            [ HH.img
                [ HP.class_ $ HH.ClassName "top-bar-game-icon"
                , HP.src $ "/images/" <> game.handle <> "/icon-white.png"
                ]
            , HH.text game.title
            ]
        )
    ]

loadGames :: ∀ left. Async left (Maybe ViewAllGames.OkContent)
loadGames = Async.unify do
    response <- Fetch.fetch_ "/api/games" # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

loadGame :: ∀ left. String -> Async left (Maybe ViewGame.OkContent)
loadGame handle = Async.unify do
    response <- Fetch.fetch_ ("/api/games/" <> handle) # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

handleAction :: ∀ action slots left.
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

component :: ∀ query left.
    H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: { selected: _, games: [] }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

gameInput
    :: ∀ action slots left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (gameInput :: Slot | slots) (Async left)
gameInput input handleOutput =
    HH.slot (Proxy :: _ "gameInput") unit component input handleOutput
