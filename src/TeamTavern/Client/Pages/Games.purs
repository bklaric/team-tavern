module TeamTavern.Client.Pages.Games where

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
import TeamTavern.Client.Components.Ads (descriptionLeaderboard, stickyLeaderboards)
import TeamTavern.Client.Components.Divider (whiteDivider)
import TeamTavern.Client.Components.Picture (picture)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.ViewAllGames as ViewAllGames
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action = Init | Navigate String Boolean MouseEvent

data State = Empty | Games ViewAllGames.OkContent

type Slot = H.Slot (Const Void) Void

render :: forall slots monad. MonadEffect monad => State -> H.ComponentHTML Action slots monad
render Empty = HH.div_ []
render (Games games') = HH.div [ HS.class_ "games" ] $
    [ HH.div [ HS.class_ "games-header"]
        [ HH.h1 [ HS.class_ "games-header-title" ]
            [ HH.text "Games" ]
        , HH.p [ HS.class_ "games-header-subtitle" ]
            [ HH.text "Choose one of the featured games and start finding your new teammates!" ]
        , descriptionLeaderboard
        ]
    ]
    <>
    (games' <#> \{ title, handle, description } ->
        HH.div
        [ HS.class_ "game-card"
        , HE.onClick $ Just <<< Navigate ("/games/" <> handle) false
        ]
        [ HH.div [ HS.class_ "game-card-text" ] $
            [ HH.h2 [ HS.class_ "game-card-heading" ]
                [ HH.a
                    [ HP.class_ $ ClassName "game-card-name"
                    , HP.href $ "/games/" <> handle <> "/players"
                    , HE.onClick $ Just <<< Navigate ("/games/" <> handle) true
                    ]
                    [ HH.img
                        [ HS.class_ "game-card-logo"
                        , HP.src $ "/images/" <> handle <> "/icon-white.png"
                        , HP.alt $ title <> " icon"
                        ]
                    , HH.text title
                    ]
                ]
            , HH.span [ HS.class_ "game-card-profiles" ]
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
                , whiteDivider
                , HH.h3 [ HP.class_ $ ClassName "game-card-profile-count" ]
                    [ HH.a
                        [ HP.href $ "/games/" <> handle <> "/competitions"
                        , HE.onClick $ Just <<< Navigate ("/games/" <> handle <> "/competitions") true
                        ]
                        [ HH.text "Competitions" ]
                    ]
                ]
            ]
            <> (description <#> \paragraph ->
                HH.p [ HS.class_ "game-card-description" ] [ HH.text paragraph ]
            )
        , picture "game-card-image" (title <> " banner") ("/images/" <> handle <> "/banner")
        ]
    )
    <> stickyLeaderboards

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
        Games games' -> setMeta "Games | TeamTavern"
            (  "Find players and teams looking for teammates for featured games on TeamTavern, an esports team finding platform."
            <> " Create your own player or team profile and let them find you."
            )
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
