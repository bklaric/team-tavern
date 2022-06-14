module TeamTavern.Client.Pages.Player.PlayerOptions (Slot, playerOptions) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Pages.Player.DeleteAccount (deleteAccount)
import TeamTavern.Client.Snippets.Class as HS

type Input = String

type Slot = H.Slot (Const Void) Void Unit

component :: forall query output left. H.Component query Input output (Async left)
component = Hooks.component $ \_ nickname -> Hooks.do
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
            [ deleteAccount
                deleteModalInput
                (const $ Hooks.put deleteModalShownId Nothing)
            ])
            deleteModalShown
        )
        [ popoverItem
            (const $ Hooks.put deleteModalShownId $ Just nickname)
            [ HH.span [ HS.class_ "delete-account-option" ]
                [ HH.i [ HS.class_ "fas fa-trash button-icon" ] [], HH.text "Delete account" ]
            ]
        ]

playerOptions :: forall action slots left.
    Input -> HH.ComponentHTML action (playerOptions :: Slot | slots) (Async left)
playerOptions nickname = HH.slot (Proxy :: _ "playerOptions") unit component nickname absurd
