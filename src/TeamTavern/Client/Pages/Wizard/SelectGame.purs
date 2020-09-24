module TeamTavern.Client.Pages.Wizard.SelectGame where

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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Game.ViewAll.SendResponse as ViewAll

type Handle = String

type Input = Maybe View.OkContent

type Output = View.OkContent

type State =
    { games :: ViewAll.OkContent
    , selected :: Maybe View.OkContent
    }

data Action = Initialize | SelectGame ViewAll.OkContent'

type Slot = H.Slot (Const Void) Output Unit

render :: forall slots. State -> HH.HTML slots Action
render { games, selected } =
    HH.div_
    [ HH.div [ HP.class_ $ HH.ClassName "select-game-list" ] $
        (games <#> \game ->
            HH.div
            [ HP.class_ $ HH.ClassName
                case selected of
                Just selected' | game.handle == selected'.handle ->
                    "select-game-selected-choice"
                _ -> "select-game-choice"
            , HE.onClick $ const $ Just $ SelectGame game
            ]
            [ HH.div [ HP.class_ $ HH.ClassName "select-game-choice-ribbon" ]
                [ HH.span [ HP.class_ $ HH.ClassName "select-game-choice-title" ]
                    [ HH.img
                        [ HP.class_ $ HH.ClassName "top-bar-game-icon"
                        , HP.src $ "/static/" <> game.handle <> "-icon.png"
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

loadGames :: forall left. Async left (Maybe ViewAll.OkContent)
loadGames = Async.unify do
    response <- Fetch.fetch_ "/api/games" # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

loadGame :: forall left. String -> Async left (Maybe View.OkContent)
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
    game' <- H.lift $ loadGame game.handle
    case game' of
        Nothing -> pure unit
        Just game'' -> do
            H.modify_ _ { selected = Just game'' }
            H.raise game''

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

selectGame
    :: forall action slots left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (selectGame :: Slot | slots) (Async left)
selectGame input handleOutput =
    HH.slot (SProxy :: SProxy "selectGame") unit component input handleOutput
