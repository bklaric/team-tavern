module TeamTavern.Client.Pages.Player.Details (details) where

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
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import Type.Proxy (Proxy(..))

details
    :: ∀ action children left
    .  ViewPlayer.OkContent
    -> Status
    -> action
    -> H.ComponentHTML action (discordTag :: Slot__String | children) (Async left)
details player status showEditPlayerModal = let
    playerDetails' = playerDetails
        ( player
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
        guard (status == SignedInSelf)
        [ regularButton "fas fa-user-edit" "Edit details" showEditPlayerModal ]
    , cardSection
        if Array.null playerDetails'
        then Array.singleton $ missing
            case status of
            SignedInSelf -> "Apparently, you prefer to keep an air of mystery about yourself."
            _ -> "Apparently, this player prefers to keep an air of mystery about them."
        else playerDetails'
    ]
