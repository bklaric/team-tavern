module TeamTavern.Client.Components.Team.SizeInfo (Slot, sizeInfo) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (togglePopover, usePopover)
import TeamTavern.Client.Components.Team.Info (info, infoContainer, infoIcon, infoText)

type Slot = H.Slot (Const Void) Void Unit

component :: forall left output input query. H.Component HH.HTML query input output (Async left)
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

sizeInfo :: forall action slots left. H.ComponentHTML action (sizeInfo :: Slot | slots) (Async left)
sizeInfo = HH.slot (SProxy :: _ "sizeInfo") unit component unit absurd
