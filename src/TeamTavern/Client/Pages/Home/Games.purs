module TeamTavern.Client.Pages.Home.Games where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorClassed)
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
    , HH.div [ HP.class_ $ HH.ClassName "call-to-action-buttons" ]
        [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-button-group" ]
            [ navigationAnchorClassed (SProxy :: SProxy "viewAllGames")
                { class_: "call-to-action-secondary-button"
                , path: "/games"
                , content: HH.text "View all games"
                }
            , HH.p_ [ HH.text "View all featured games and start your team finding journey." ]
            ]
        ]
    ]
