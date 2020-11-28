module TeamTavern.Client.Pages.Team.Details (details) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Record as Record
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading3, detailColumns, detailColumnsContainer, textDetail)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Pages.Team.Status (Status(..))
import TeamTavern.Server.Team.View (Team)

details :: forall action slots left.
    Team -> Status -> H.ComponentHTML action slots (Async left)
details team status = let
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
    about = textDetail team.about
    in
    card
    [ cardHeader [ cardHeading "Team" ]
    , cardSection
        if Array.null teamDetails' && Array.null about
        then Array.singleton $ missing
            case status of
            SignedInOwner -> "Apparently, your team prefers to keep an air of mystery about them."
            _ -> "Apparently, this team prefers to keep an air of mystery about them."
        else Array.singleton $ detailColumnsContainer $ Array.singleton $ detailColumns $
            ( if Array.null teamDetails'
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading3 "Details" ] <> teamDetails'
            )
            <>
            ( if Array.null about
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading3 "About" ] <> about
            )
    ]
