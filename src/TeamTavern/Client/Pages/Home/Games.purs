module TeamTavern.Client.Pages.Home.Games (games) where

import Prelude

import Async (Async)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isNothing)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.GameCover (gameCover)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyUnavailable)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

type State = Maybe ViewAllGames.OkContent

data Action = Initialize | Navigate String MouseEvent

gameTile :: ∀ slots. ViewAllGames.OkGameContent -> HH.HTML slots Action
gameTile game @ { handle } =
    HH.div [ HS.class_ "home-game" ]
    [ HH.a
        [ HS.class_ "home-game-cover"
        , HP.href players
        , HE.onClick $ Navigate players
        ]
        [ gameCover game ]
    , HH.div [ HS.class_ "home-game-links" ]
        [ HH.a [ HP.href players, HE.onClick $ Navigate players ] [ HH.text "Players" ]
        , divider
        , HH.a [ HP.href teams, HE.onClick $ Navigate teams ] [ HH.text "Teams" ]
        ]
    ]
    where
    players = "/games/" <> handle <> "/players"
    teams = "/games/" <> handle <> "/teams"

render :: ∀ slots. State -> HH.HTML slots Action
render games' =
    HH.div [ HP.id "games", HS.class_ "landing-section home-games" ]
    [ HH.h2 [ HS.class_ "home-games-heading" ] [ HH.text "Pick your game" ]
    , HH.div [ HS.class_ "home-games-grid" ] $ foldMap (_ <#> gameTile) games'
    ]

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    games' <- H.lift $ get "/api/games"
    when (isNothing games') appendRenderReadyUnavailable
    H.put games'
handleAction (Navigate url event) = navigateWithEvent_ url event

component :: ∀ query input output left. H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

games :: ∀ query children left.
    HH.ComponentHTML query (games :: Slot___ | children) (Async left)
games = HH.slot (Proxy :: _ "games") unit component unit absurd
