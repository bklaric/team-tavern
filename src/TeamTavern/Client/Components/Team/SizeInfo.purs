module TeamTavern.Client.Components.Team.SizeInfo (sizeInfo) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (togglePopover, usePopover)
import TeamTavern.Client.Components.Team.Info (info, infoContainer, infoIcon, infoText)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import Type.Proxy (Proxy(..))

component :: ∀ left output input query. H.Component query input output (Async left)
component = Hooks.component $ \_ _ -> Hooks.do
    (Tuple shown shownId) <- usePopover

    Hooks.pure $
        infoContainer $
        [ infoIcon $ togglePopover shownId ]
        <> (guard shown $ Array.singleton $
            info
            [ infoText "Choose party if you are recruiting a smaller number of players to fill out a team, roster or group."
            , infoText "Choose community if you are recruiting a larger number of players for a guild, clan or another type of gaming community."
            ]
        )

sizeInfo :: ∀ action slots left. H.ComponentHTML action (sizeInfo :: SimpleSlot | slots) (Async left)
sizeInfo = HH.slot (Proxy :: _ "sizeInfo") unit component unit absurd
