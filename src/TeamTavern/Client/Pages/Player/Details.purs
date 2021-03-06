module TeamTavern.Client.Pages.Player.Details (details) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Record as Record
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading3, detailColumns, detailColumnsContainer, textDetail)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Routes.ViewPlayer as ViewPlayer

details
    :: forall action children left
    .  ViewPlayer.OkContent
    -> Status
    -> action
    -> H.ComponentHTML action (discordTag :: Copyable.Slot String | children) (Async left)
details player status showEditPlayerModal = let
    playerDetails' = playerDetails
        ( player
        # Record.modify (SProxy :: SProxy "weekdayOnline")
            case _ of
            Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
            Nothing -> Nothing
        # Record.modify (SProxy :: SProxy "weekendOnline")
            case _ of
            Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
            Nothing -> Nothing
        )
    about = textDetail player.about
    in
    card
    [ cardHeader $
        [ cardHeading "Player" ]
        <>
        case status of
        SignedInSelf -> [ regularButton "fas fa-user-edit" "Edit player" showEditPlayerModal ]
        _ -> []
    , cardSection
        if Array.null playerDetails' && Array.null about
        then Array.singleton $ missing
            case status of
            SignedInSelf -> "Apparently, you prefer to keep an air of mystery about yourself."
            _ -> "Apparently, this player prefers to keep an air of mystery about them."
        else Array.singleton $ detailColumnsContainer $ Array.singleton $ detailColumns $
            ( if Array.null playerDetails'
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading3 "Details" ] <> playerDetails'
            )
            <>
            ( if Array.null about
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading3 "About" ] <> about
            )
    ]
