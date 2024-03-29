module TeamTavern.Client.Pages.Player.Teams (teams) where

import Prelude

import Data.Array as Array
import Data.Monoid (guard)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (primaryButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Profile (profileHeading, profileSubheading)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Pages.Profiles.TeamBadge (informalBadge, organizedBadge)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Routes.Shared.Organization (OrganizationN(..), nameOrHandleN)
import Type.Proxy (Proxy(..))

teams
    :: ∀ action monad slots
    .  MonadEffect monad
    => ViewPlayer.OkContent
    -> Status
    -> action
    -> HH.ComponentHTML action (team :: Slot__String | slots) monad
teams player status showCreateTeamModal =
    card $
    [ cardHeader $
        [ cardHeading "Teams" ]
        <>
        guard (status == SignedInSelf)
        [ primaryButton "fas fa-user-plus" "Create team" showCreateTeamModal ]
    ]
    <>
    if Array.null player.teams
    then Array.singleton $
        cardSection
        [ missing
            case status of
            SignedInSelf -> "You haven't created any teams."
            _ -> "This player hasn't created any teams."
        ]
    else
        player.teams <#> \team ->
            cardSection
            [ HH.div [ HS.class_ "team-profile-heading-container" ]
                [ profileHeading (Proxy :: _ "team") team.handle
                    ("/teams/" <> team.handle) (nameOrHandleN team.handle team.organization)
                , case team.organization of
                    InformalN -> informalBadge
                    OrganizedN _ -> organizedBadge
                , profileSubheading $ "Updated " <> lastUpdated team.updatedSeconds
                ]
            ]
