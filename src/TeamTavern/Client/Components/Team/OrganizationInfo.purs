module TeamTavern.Client.Components.Team.OrganizationInfo (Slot, organizationInfo) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (togglePopover, usePopover)
import TeamTavern.Client.Snippets.Class as HS

type Slot = H.Slot (Const Void) Void Unit

component :: forall left output input query. H.Component HH.HTML query input output (Async left)
component = Hooks.component $ \_ _ -> Hooks.do
    (Tuple shown shownId) <- usePopover

    Hooks.pure $
        HH.div [ HS.class_ "organization-info-container" ] $
        [ HH.i
            [ HS.class_ "fas fa-question-circle organization-info-icon"
            , HP.onClick $ Just <<< togglePopover shownId
            ]
            []
        ]
        <> (guard shown $ Array.singleton $
            HH.div [ HS.class_ "organization-info" ]
            [ HH.p [ HS.class_ "organization-info-intro" ] [ HH.text "You are an informal team if you:" ]
            , HH.ul [ HS.class_ "organization-info-list" ]
                [ HH.li_ [ HH.text "Don't have a name, a logo or a chat server." ]
                , HH.li_ [ HH.text "Don't have strict membership rules that need to be followed." ]
                , HH.li_ [ HH.text "Don't have a defined governance structure." ]
                ]
            , HH.p [ HS.class_ "organization-info-intro" ] [ HH.text "Informal team exaples:" ]
            , HH.ul [ HS.class_ "organization-info-list" ]
                [ HH.li_ [ HH.text "Parties for climbing ranked ladders in games." ]
                , HH.li_ [ HH.text "Unmoderated or lightly moderated game servers." ]
                ]
            , HH.p [ HS.class_ "organization-info-intro" ] [ HH.text "You are an organized team if you:" ]
            , HH.ul [ HS.class_ "organization-info-list" ]
                [ HH.li_ [ HH.text "Have a name, a logo, a chat server, maybe even a website." ]
                , HH.li_ [ HH.text "Have membership rules, such as minimum active in-game time, gaming nights, training schedule etc." ]
                , HH.li_ [ HH.text "Have some form of governance structure, such as administrators, moderators, roles etc." ]
                ]
            , HH.p [ HS.class_ "organization-info-intro" ] [ HH.text "Organized team exaples:" ]
            , HH.ul [ HS.class_ "organization-info-list" ]
                [ HH.li_ [ HH.text "Amateur or professional teams competing in leagues and tournaments." ]
                , HH.li_ [ HH.text "Guilds, clans and other moderated gaming communities." ]
                ]
            ]
        )

organizationInfo :: forall action slots left. H.ComponentHTML action (organizationInfo :: Slot | slots) (Async left)
organizationInfo = HH.slot (SProxy :: _ "organizationInfo") unit component unit absurd
