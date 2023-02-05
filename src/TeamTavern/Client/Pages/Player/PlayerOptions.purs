module TeamTavern.Client.Pages.Player.PlayerOptions (playerOptions) where

import Prelude

import Async (Async)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Pages.Player.ChangeEmail (changeEmail)
import TeamTavern.Client.Pages.Player.ChangePassword (changePassword)
import TeamTavern.Client.Pages.Player.DeleteAccount (deleteAccount)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))

type Input = {email :: Maybe String, nickname :: String}

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component $ \_ {email, nickname} -> Hooks.do
    shown /\ shownId <- usePopover
    changeEmailModalShown /\ changeEmailModalShownId <- Hooks.useState false
    changePasswordModalShown /\ changePasswordModalShownId <- Hooks.useState false
    deleteModalShown /\ deleteModalShownId <- Hooks.useState Nothing
    Hooks.pure $
        popover
        shown
        ([ HH.i
            [ HS.class_ "fas fa-ellipsis-h options-button-icon"
            , HE.onClick $ togglePopover shownId
            ]
            []
        ]
        <> guard changeEmailModalShown
            [ changeEmail {email, nickname}
                (const $ Hooks.put changeEmailModalShownId false)
            ]
        <> guard changePasswordModalShown
            [ changePassword {nickname}
                (const $ Hooks.put changePasswordModalShownId false)
            ]
        <> foldMap (\deleteModalInput ->
            [ deleteAccount
                deleteModalInput
                (const $ Hooks.put deleteModalShownId Nothing)
            ])
            deleteModalShown
        )
        [ popoverItem
            (const $ Hooks.put changeEmailModalShownId true)
            [ HH.text "Change email" ]
        , popoverItem
            (const $ Hooks.put changePasswordModalShownId true)
            [ HH.text "Change password" ]
        , popoverItem
            (const $ Hooks.put deleteModalShownId $ Just nickname)
            [ HH.span [ HS.class_ "delete-account-option" ]
                [ HH.text "Delete account" ]
            ]
        ]

playerOptions :: ∀ action slots left.
    Input -> HH.ComponentHTML action (playerOptions :: Slot___ | slots) (Async left)
playerOptions nickname = HH.slot (Proxy :: _ "playerOptions") unit component nickname absurd
