module TeamTavern.Client.Pages.Home.Wizard.SelectGame where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Pages.Home.Wizard.Shared (Ilk)
import TeamTavern.Server.Game.ViewAll.SendResponse (OkContent, OkContent')

type Handle = String

type Game = OkContent'

type Games = OkContent

type Input = { ilk :: Ilk, selectedHandle :: Maybe Handle }

type Output = Handle

type State =
    { ilk :: Ilk
    , selectedHandle :: Maybe Handle
    , games :: Games
    , selectedGame :: Maybe Game
    }

data Action = Initialize | SelectGame Game

type Slot = H.Slot (Const Void) Output Unit

render :: forall slots. State -> HH.HTML slots Action
render { ilk, games, selectedGame } =
    HH.div_
    [ HH.div [ HP.class_ $ HH.ClassName "select-game-list" ] $
        (games <#> \game ->
            HH.div
            [ HP.class_ $ HH.ClassName
                case selectedGame of
                Just selectedGame' | game.handle == selectedGame'.handle ->
                    "select-game-selected-choice"
                _ -> "select-game-choice"
            , HE.onClick $ const $ Just $ SelectGame game
            ]
            [ HH.div [ HP.class_ $ HH.ClassName "select-game-choice-ribbon" ]
                [ HH.span [ HP.class_ $ HH.ClassName "select-game-choice-title" ]
                    [ HH.img
                        [ HP.class_ $ HH.ClassName "top-bar-game-icon"
                        , HP.src $ "/images/" <> game.handle <> "-icon.png"
                        ]
                    , HH.text game.title
                    ]
                ]
            ])
        <>
        [ HH.div [ HP.class_ $ HH.ClassName "select-game-choice" ]
            [ HH.div [ HP.class_ $ HH.ClassName "select-game-choice-ribbon" ]
                [ HH.span [ HP.class_ $ HH.ClassName "select-game-choice-title" ]
                    [ HH.text "More games coming soon!" ]
                ]
            ]
        ]
    ]

loadGames :: forall left. Async left (Maybe Games)
loadGames = Async.unify do
    response <- Fetch.fetch_ "/api/games" # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

handleAction :: forall action slots left.
    Action -> H.HalogenM State action slots Output (Async left) Unit
handleAction Initialize = do
    games <- H.lift $ maybe [] identity <$> loadGames
    H.modify_ \state -> state
        { games = games
        , selectedGame = state.selectedHandle >>= \handle ->
            games # find (_.handle >>> (_ == handle))
        }
handleAction (SelectGame game) = do
    H.modify_ _ { selectedGame = Just game }
    H.raise game.handle

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \{ ilk, selectedHandle } ->
        { ilk, selectedHandle, games: [], selectedGame: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

selectGame
    :: forall action slots left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (selectGame :: Slot | slots) (Async left)
selectGame input handleOutput =
    HH.slot (SProxy :: SProxy "selectGame") unit component input handleOutput
