module TeamTavern.Client.Components.Team.OrganizationInfo (organizationInfo) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (togglePopover, usePopover)
import TeamTavern.Client.Components.Team.Info (info, infoContainer, infoIcon, infoList, infoText)
import TeamTavern.Client.Shared.Slot (Slot___)
import Type.Proxy (Proxy(..))

component :: ∀ left output input query. H.Component query input output (Async left)
component = Hooks.component $ \_ _ -> Hooks.do
    (Tuple shown shownId) <- usePopover

    Hooks.pure $
        infoContainer $
        [ infoIcon $ togglePopover shownId ]
        <> (guard shown $ Array.singleton $
            info
            [ infoText "You are an informal team if you:"
            , infoList
                [ "Don't have a name or a logo."
                , "Don't have strict membership rules that need to be followed."
                , "Don't have a defined governance structure."
                ]
            , infoText "Informal team examples:"
            , infoList
                [ "Parties for climbing ranked ladders in games."
                , "Unmoderated or lightly moderated game servers."
                ]
            , infoText "You are an organized team if you:"
            , infoList
                [ "Have a name, a logo and maybe even a website."
                , "Have membership rules, such as minimum active in-game time, gaming nights, training schedule etc."
                , "Have some form of governance structure, such as administrators, moderators, roles etc."
                ]
            , infoText "Organized team examples:"
            , infoList
                [ "Amateur or professional teams competing in leagues and tournaments."
                , "Guilds, clans and other moderated gaming communities."
                ]
            ]
        )

organizationInfo :: ∀ action slots left. H.ComponentHTML action (organizationInfo :: Slot___ | slots) (Async left)
organizationInfo = HH.slot (Proxy :: _ "organizationInfo") unit component unit absurd
