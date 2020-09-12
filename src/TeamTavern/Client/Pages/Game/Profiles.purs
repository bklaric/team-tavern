module TeamTavern.Client.Pages.Game.Profiles where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorClassed)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor

type Input = { handle :: String, title :: String }

type Slots slots =
    ( viewAllPlayers :: NavigationAnchor.Slot Unit
    , viewAllTeams :: NavigationAnchor.Slot Unit
    | slots )

profiles :: forall slots action monad. MonadEffect monad =>
    Input -> HH.ComponentHTML action (Slots slots) monad
profiles { handle, title } =
    HH.div [ HP.class_ $ HH.ClassName "why" ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "why-heading" ]
        [ HH.text "Browse players and teams" ]
    , HH.div [ HP.class_ $ HH.ClassName "call-to-action-buttons" ]
        [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-button-group" ]
            [ navigationAnchorClassed (SProxy :: SProxy "viewAllPlayers")
                { class_: "call-to-action-secondary-button"
                , path: "/games/" <> handle <> "/players"
                , content: HH.text "View all players"
                }
            , HH.p_ [ HH.text $ "View all " <> title <> " players looking for a team." ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "call-to-action-button-group" ]
            [ navigationAnchorClassed (SProxy :: SProxy "viewAllTeams")
                { class_: "call-to-action-secondary-button"
                , path: "/games/" <> handle <> "/teams"
                , content: HH.text "View all teams"
                }
            , HH.p_ [ HH.text $ "View all " <> title <> " teams looking for players." ]
            ]
        ]
    ]
