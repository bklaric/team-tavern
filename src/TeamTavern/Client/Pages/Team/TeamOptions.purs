module TeamTavern.Client.Pages.Team.TeamOptions (teamOptions) where

import Prelude

import Async (Async)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Pages.Team.DeleteTeam (deleteTeam)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import Type.Proxy (Proxy(..))

component :: ∀ query output left.
    H.Component query ViewTeam.OkContent output (Async left)
component = Hooks.component $ \_ team -> Hooks.do
    (Tuple shown shownId) <- usePopover
    (Tuple deleteModalShown deleteModalShownId) <- Hooks.useState Nothing
    Hooks.pure $
        popover
        shown
        ([ HH.i
            [ HS.class_ "fas fa-ellipsis-h options-button-icon"
            , HE.onClick $ togglePopover shownId
            ]
            []
        ]
        <> foldMap (\deleteModalInput ->
            [ deleteTeam
                deleteModalInput
                (const $ Hooks.put deleteModalShownId Nothing)
            ])
            deleteModalShown
        )
        [ popoverItem
            (const $ Hooks.put deleteModalShownId $ Just team)
            [ HH.span [ HS.class_ "delete-account-option" ]
                [ HH.i [ HS.class_ "fas fa-trash button-icon" ] []
                , HH.text "Delete team"
                ]
            ]
        ]

teamOptions
    :: ∀ action slots left
    .  ViewTeam.OkContent
    -> HH.ComponentHTML action (teamOptions :: Slot___ | slots) (Async left)
teamOptions handle = HH.slot (Proxy :: _ "teamOptions") unit
    component handle absurd
