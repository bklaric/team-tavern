module TeamTavern.Client.Home.Games (Slot, games) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (whiteDivider)
import TeamTavern.Client.Script.Cookie (Nickname)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Server.Game.ViewAll.SendResponse (OkContent)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action = Init (Maybe Nickname) | Navigate String Boolean MouseEvent

data State = Empty | Games (Maybe Nickname) OkContent

type Slot = H.Slot (Const Void) Void

render :: forall slots monad. MonadEffect monad =>
    State -> H.ComponentHTML Action slots monad
render Empty = HH.div_ []
render (Games nickname games') = HH.div [ HP.class_ $ HH.ClassName "games" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "choose-game" ]
        [ HH.text
            case nickname of
            Nothing -> "Looking for players? Choose a game below and browse player profiles"
            Just nickname' ->
                "Hi " <> nickname'
                <> ", choose a game below and browse player profiles"
        ]
    ]
    <>
    (games' <#> \{ title, handle, description, playerCount, teamCount } ->
        HH.div
        [ HP.class_ $ HH.ClassName "game-card"
        , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/players") false
        ] $
        [ HH.h3 [ HP.class_ $ HH.ClassName "game-card-heading" ]
            [ HH.a
                [ HP.class_ $ ClassName "game-card-name"
                , HP.href $ "/games/" <> handle <> "/players"
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/players") true
                ]
                [ HH.img
                    [ HP.class_ $ HH.ClassName "game-card-logo"
                    , HP.src "/static/dota2-icon.svg"
                    , HP.alt "Dota 2 icon"
                    ]
                , HH.text title
                ]
            , whiteDivider
            , HH.a
                [ HP.class_ $ ClassName "game-card-profile-count"
                , HP.href $ "/games/" <> handle <> "/players"
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/players") true
                ]
                [ HH.text $ show playerCount <> if playerCount == 1 then " player" else " players" ]
            , whiteDivider
            , HH.a
                [ HP.class_ $ ClassName "game-card-profile-count"
                , HP.href $ "/games/" <> handle <> "/teams"
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/teams") true
                ]
                [ HH.text $ show teamCount <> if teamCount == 1 then " team" else " teams" ]
            ]
        ]
        <> (description <#> \paragraph ->
            HH.p [ HP.class_ $ HH.ClassName "game-card-description" ]
            [ HH.text paragraph ]
        )
    )

loadGames :: forall left. Maybe Nickname -> Async left State
loadGames nickname = Async.unify do
    response' <- Fetch.fetch_ "/api/games" # lmap (const Empty)
    games' :: OkContent <-
        case FetchRes.status response' of
        200 -> FetchRes.text response'
            >>= JsonAsync.readJSON
            # lmap (const Empty)
        _ -> Async.left Empty
    pure $ Games nickname games'

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction (Init nickname) = do
    newState <- H.lift $ loadGames nickname
    H.put newState
handleAction (Navigate url stopBubble event) = do
    H.liftEffect if stopBubble
        then stopPropagation $ MouseEvent.toEvent event
        else pure unit
    H.liftEffect $ navigateWithEvent_ url event

component :: forall query output left.
    Maybe Nickname -> H.Component HH.HTML query State output (Async left)
component nickname =
    H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just $ Init nickname
            }
        }

games
    :: forall query children left
    .  Maybe Nickname
    -> HH.ComponentHTML query (games :: Slot Unit | children) (Async left)
games nickname =
    HH.slot (SProxy :: SProxy "games") unit (component nickname) Empty absurd
