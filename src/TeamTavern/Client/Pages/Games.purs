module TeamTavern.Client.Pages.Games where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Ads (insertAdsInMiddle, mobileBanner)
import TeamTavern.Client.Components.Content (actualContent)
import TeamTavern.Client.Components.Divider (whiteDivider)
import TeamTavern.Client.Components.Picture (picture)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import Type.Proxy (Proxy(..))
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Yoga.JSON.Async as JsonAsync

data Action = Init | Navigate String Boolean MouseEvent

data State = Empty | Games ViewAllGames.OkContent

render :: ∀ slots monad. MonadEffect monad => State -> H.ComponentHTML Action slots monad
render Empty = HH.div_ []
render (Games games') = actualContent $
    [ HH.h1 [ HS.class_ "games-header-title" ]
        [ HH.text "Games" ]
    , HH.p [ HS.class_ "games-header-subtitle" ]
        [ HH.text "Choose one of the featured games and start finding your new teammates!" ]
    , mobileBanner
    ]
    <> insertAdsInMiddle
    (games' <#> \{ title, handle, description } ->
        HH.div
        [ HS.class_ "game-card"
        , HE.onClick $ Navigate ("/games/" <> handle) false
        ]
        [ HH.div [ HS.class_ "game-card-text" ] $
            [ HH.h2 [ HS.class_ "game-card-heading" ]
                [ HH.a
                    [ HP.class_ $ ClassName "game-card-name"
                    , HP.href $ "/games/" <> handle <> "/players"
                    , HE.onClick $ Navigate ("/games/" <> handle) true
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
                        , HE.onClick $ Navigate ("/games/" <> handle <> "/players") true
                        ]
                        [ HH.text "Players" ]
                    ]
                , whiteDivider
                , HH.h3 [ HP.class_ $ ClassName "game-card-profile-count" ]
                    [ HH.a
                        [ HP.href $ "/games/" <> handle <> "/teams"
                        , HE.onClick $ Navigate ("/games/" <> handle <> "/teams") true
                        ]
                        [ HH.text "Teams" ]
                    ]
                ]
            ]
            <> (description <#> \paragraph ->
                HH.p [ HS.class_ "game-card-description" ] [ HH.text paragraph ]
            )
        , picture "game-card-image" (title <> " banner") ("/images/" <> handle <> "/banner")
        ]
    )

loadGames :: ∀ left. Async left State
loadGames = Async.unify do
    response' <- Fetch.fetch_ "/api/games" # lmap (const Empty)
    games' :: ViewAllGames.OkContent <-
        case FetchRes.status response' of
        200 -> FetchRes.text response'
            >>= JsonAsync.readJSON
            # lmap (const Empty)
        _ -> Async.left Empty
    pure $ Games games'

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Init = do
    newState <- H.lift loadGames
    H.put newState
    case newState of
        Games _ -> setMeta "Games | TeamTavern"
            (  "Find players and teams looking for teammates for featured games on TeamTavern, an esports team finding platform."
            <> " Create your own player or team profile and let them find you."
            )
        _ -> pure unit
handleAction (Navigate url stopBubble event) = do
    H.liftEffect if stopBubble
        then stopPropagation $ MouseEvent.toEvent event
        else pure unit
    navigateWithEvent_ url event

component :: ∀ query output left.
    H.Component query State output (Async left)
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
    :: ∀ query children left
    .  HH.ComponentHTML query (games :: Slot___ | children) (Async left)
games =
    HH.slot (Proxy :: _ "games") unit component Empty absurd
