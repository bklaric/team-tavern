module TeamTavern.Client.Pages.Team.Details (details) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen as H
import Record as Record
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Pages.Team.Status (Status(..))
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import Type.Proxy (Proxy(..))

details
    :: ∀ action slots left
    .  ViewTeam.OkContent
    -> Status
    -> action
    -> H.ComponentHTML action (discordTag :: Slot__String | slots) (Async left)
details team status showEditTeamModal = let
    teamDetails' = teamDetails
        ( team
        # Record.modify (Proxy :: _ "weekdayOnline")
            case _ of
            Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
            Nothing -> Nothing
        # Record.modify (Proxy :: _ "weekendOnline")
            case _ of
            Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
            Nothing -> Nothing
        )
    in
    card
    [ cardHeader $
        [ cardHeading "Details" ]
        <>
        guard (status == SignedInOwner)
        [ regularButton "fas fa-edit" "Edit team" showEditTeamModal ]
    , cardSection
        if Array.null teamDetails'
        then Array.singleton $ missing
            case status of
            SignedInOwner -> "Apparently, your team prefers to keep an air of mystery about them."
            _ -> "Apparently, this team prefers to keep an air of mystery about them."
        else teamDetails'
    ]
