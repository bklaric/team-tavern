module TeamTavern.Client.Pages.Home.Games where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor

games :: forall slots action monad. MonadEffect monad =>
    HH.ComponentHTML action (viewAllGames :: NavigationAnchor.Slot Unit | slots) monad
games =
    HH.div [ HP.class_ $ HH.ClassName "why" ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "why-heading" ]
        [ HH.text "Diverse games" ]
    , HH.p [ HP.class_ $ HH.ClassName "why-reason-description" ]
        [ HH.text """TeamTavern aims to provide a wide selection of games while
        carefully adjusting to every game's specifics.""" ]
    , HH.p [ HP.class_ $ HH.ClassName "why-reason-description" ]
        [ navigationAnchor (SProxy :: SProxy "viewAllGames")
            { path: "/games", content: HH.text "View all featured games" }
        , HH.text """ and start your team finding journey."""
        ]
    ]
