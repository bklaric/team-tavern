module TeamTavern.Client.Pages.Player.Teams (teams) where

import Prelude

import Data.Array as Array
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (primaryButton)
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
    -> Status
    -> action
    -> HH.ComponentHTML action (team :: NavigationAnchor.Slot String | slots) monad
teams player status showCreateTeamModal =
    card $
    [ cardHeader $
        [ cardHeading "Teams" ]
        <>
        case status of
        SignedInSelf -> Array.singleton $
            primaryButton "fas fa-user-plus" "Create team" showCreateTeamModal
        _ -> []
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
