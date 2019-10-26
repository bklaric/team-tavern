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
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Server.Game.ViewAll.SendResponse (OkContent)
import Web.UIEvent.MouseEvent (MouseEvent)

data Action = Init | Navigate String MouseEvent

data State = Empty | Games OkContent

type Slot = H.Slot (Const Void) Void

render :: forall slots monad. MonadEffect monad =>
    State -> H.ComponentHTML Action slots monad
render Empty = HH.div_ []
render (Games games') = HH.div [ HP.class_ $ HH.ClassName "games" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "choose-game" ]
        [ HH.text "Choose a game below and browse player profiles" ]
    ]
    <>
    (games' <#> \{ title, handle, description, profileCount } ->
        HH.div
        [ HP.class_ $ HH.ClassName "game-card"
        , HE.onClick $ Just <<< Navigate ("/games/" <> handle)
        ] $
        [ HH.h2 [ HP.class_ $ HH.ClassName "game-card-heading" ]
            [ HH.a
                [ HP.class_ $ ClassName "game-card-name"
                , HP.href $ "/games/" <> handle
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle)
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
                , HP.href $ "/games/" <> handle
                , HE.onClick $ Just <<< Navigate ("/games/" <> handle)
                ]
                [ HH.text $ show profileCount <> " profiles" ]
            ]
        ]
        <> (description <#> \paragraph ->
            HH.p [ HP.class_ $ HH.ClassName "game-card-description" ]
            [ HH.text paragraph ]
        )
    )

loadGames :: forall left. Async left State
loadGames = Async.unify do
    response' <- Fetch.fetch_ "/api/games" # lmap (const Empty)
    games' :: OkContent <-
        case FetchRes.status response' of
        200 -> FetchRes.text response'
            >>= JsonAsync.readJSON
            # lmap (const Empty)
        _ -> Async.left Empty
    pure $ Games games'

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Init = do
    newState <- H.lift loadGames
    H.put newState
handleAction (Navigate url event) =
    H.liftEffect $ navigateWithEvent_ url event

component :: forall query output left.
    H.Component HH.HTML query State output (Async left)
component =
    H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
        }

games :: forall query children left.
    HH.ComponentHTML query (games :: Slot Unit | children) (Async left)
games = HH.slot (SProxy :: SProxy "games") unit component Empty absurd
