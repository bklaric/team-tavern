module TeamTavern.Client.Pages.Player.Details (details) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading3, detailColumns, detailColumnsContainer, textDetail)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Routes.ViewPlayer as ViewPlayer

details
    :: forall action children left
    .  ViewPlayer.OkContent
    -> H.ComponentHTML action (discordTag :: Copyable.Slot String | children) (Async left)
details player = let
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
    [ cardHeader [ cardHeading "Player" ]
    , cardSection
        if Array.null playerDetails' && Array.null about
        then [ HH.p_ [ HH.text "No details, kek." ] ]
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
