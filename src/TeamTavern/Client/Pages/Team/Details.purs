module TeamTavern.Client.Pages.Team.Details (details) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen as H
import Record as Record
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Pages.Team.Status (Status(..))
import TeamTavern.Server.Team.View (Team)

details
    :: forall action slots left
    .  Team
    -> Status
    -> action
    -> H.ComponentHTML action (discordTag :: Copyable.Slot String | slots) (Async left)
details team status showEditTeamModal = let
    teamDetails' = teamDetails
        ( team
        # Record.modify (SProxy :: SProxy "weekdayOnline")
            case _ of
            Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
            Nothing -> Nothing
        # Record.modify (SProxy :: SProxy "weekendOnline")
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
