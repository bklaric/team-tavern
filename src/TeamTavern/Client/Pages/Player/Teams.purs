module TeamTavern.Client.Pages.Player.Teams (teams) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Profile (profileHeading, profileSubheading)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Routes.ViewPlayer as ViewPlayer

teams
    :: forall action monad slots
    .  MonadEffect monad
    => ViewPlayer.OkContent
    -> action
    -> Status
    -> HH.ComponentHTML action (team :: NavigationAnchor.Slot String | slots) monad
teams player showCreateTeamModal status =
    card $
    [ cardHeader $
        [ cardHeading "Teams"
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HE.onClick $ const $ Just showCreateTeamModal
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
            , HH.text "Create team"
            ]
        ]
    ]
    <>
    if Array.null player.teams
    then Array.singleton $
        cardSection
        [ HH.p_
            [ HH.text
                case status of
                SignedInSelf -> "You haven't created any teams."
                _ -> "This player hasn't created any teams."
            ]
        ]
    else
        player.teams <#> \team ->
            cardSection
            [ profileHeading (SProxy :: SProxy "team") team.handle
                ("/teams/" <> team.handle) team.name
            , divider
            , profileSubheading $ "Updated " <> lastUpdated team.updatedSeconds
            ]
