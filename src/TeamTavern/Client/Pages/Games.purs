module TeamTavern.Client.Pages.Games where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import CSS as CSS
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (alt, class_, href, src) as HP
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (whiteDivider)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Routes.ViewAllGames as ViewAllGames
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action = Init | Navigate String Boolean MouseEvent

data State = Empty | Games ViewAllGames.OkContent

type Slot = H.Slot (Const Void) Void

render :: forall slots monad. MonadEffect monad =>
    State -> H.ComponentHTML Action slots monad
render Empty = HH.div_ []
render (Games games') = HH.div [ HP.class_ $ HH.ClassName "games" ] $
    [ HH.div [ HP.class_ $ HH.ClassName "games-header"]
        [ HH.h1 [ HP.class_ $ HH.ClassName "games-header-title" ]
            [ HH.text "Games" ]
        , HH.p [ HP.class_ $ HH.ClassName "games-header-subtitle" ]
            [ HH.text "Choose one of the featured games and start finding your new teammates!" ]
        ]
    ]
    <>
    (games' <#> \{ title, handle, description } ->
        HH.div
        [ HP.class_ $ HH.ClassName "game-card"
        , HE.onClick $ Just <<< Navigate ("/games/" <> handle) false
        ]
        [ HH.div
            [ HP.class_ $ HH.ClassName "game-card-text"
            , HP.style $ CSS.backgroundImage $ CSS.fromString $
                "linear-gradient(hsla(20, 20%, 19%, 0.8),hsla(20, 20%, 19%, 0.8)), url(/images/" <> handle <> "-banner.jpg)"
            ] $
            [ HH.h2 [ HP.class_ $ HH.ClassName "game-card-heading" ]
                [ HH.a
                    [ HP.class_ $ ClassName "game-card-name"
                    , HP.href $ "/games/" <> handle <> "/players"
                    , HE.onClick $ Just <<< Navigate ("/games/" <> handle) true
                    ]
                    [ HH.img
                        [ HP.class_ $ HH.ClassName "game-card-logo"
                        , HP.src $ "/images/" <> handle <> "-icon.png"
                        , HP.alt $ title <> " icon"
                        ]
                    , HH.text title
                    ]
                ]
            , HH.span [ HP.class_ $ HH.ClassName "game-card-profiles" ]
                [ HH.h3 [ HP.class_ $ ClassName "game-card-profile-count" ]
                    [ HH.a
                        [ HP.href $ "/games/" <> handle <> "/players"
                        , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/players") true
                        ]
                        [ HH.text "Players" ]
                    ]
                , whiteDivider
                , HH.h3 [ HP.class_ $ ClassName "game-card-profile-count" ]
                    [ HH.a
                        [ HP.href $ "/games/" <> handle <> "/teams"
                        , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/teams") true
                        ]
                        [ HH.text "Teams" ]
                    ]
                ]
            ]
            <> (description <#> \paragraph ->
                HH.p [ HP.class_ $ HH.ClassName "game-card-description" ]
                [ HH.text paragraph ]
            )
        , HH.div
            [ HP.class_ $ HH.ClassName "game-card-image"
            , HP.style $ CSS.backgroundImage $ CSS.url $ "/images/" <> handle <> "-banner.jpg"
            ]
            []
        ]
    )

loadGames :: forall left. Async left State
loadGames = Async.unify do
    response' <- Fetch.fetch_ "/api/games" # lmap (const Empty)
    games' :: ViewAllGames.OkContent <-
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
    case newState of
        Games games' -> setMeta "View all featured games | TeamTavern"
            "Choose one of the featured games and start finding your new teammates!"
        _ -> pure unit
handleAction (Navigate url stopBubble event) = do
    H.liftEffect if stopBubble
        then stopPropagation $ MouseEvent.toEvent event
        else pure unit
    navigateWithEvent_ url event

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

games
    :: forall query children left
    .  HH.ComponentHTML query (games :: Slot Unit | children) (Async left)
games =
    HH.slot (SProxy :: SProxy "games") unit component Empty absurd
