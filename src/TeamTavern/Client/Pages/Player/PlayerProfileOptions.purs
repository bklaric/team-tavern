module TeamTavern.Client.Pages.Player.PlayerProfileOptions (Output(..), Slot, playerProfileOptions) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Shared.Slot (Slot_OI)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location, open)

type Input = { status :: Status, nickname :: String, handle :: String }

data Output = Edit | Delete

type Slot = Slot_OI Output String

profileUrl :: Input -> Effect String
profileUrl { nickname, handle } = do
    origin' <- window >>= location >>= origin
    pure $ origin' <> "/players/" <> nickname <> "/profiles/" <> handle

component :: ∀ query left. H.Component query Input Output (Async left)
component = Hooks.component $ \{ outputToken } input -> Hooks.do
    (Tuple shown shownId) <- usePopover
    let openProfileInNewTab = fromEffect do
            profileUrl' <- profileUrl input
            window >>= open profileUrl' "" "" # void
            track "Profile new tab open" {ilk: "player"}
    let copyProfileAddress = do
            profileUrl' <- profileUrl input # fromEffect
            writeTextAsync profileUrl' # attempt # void
            track "Profile address copy" {ilk: "player"}
    Hooks.pure $
        popover
        shown
        [ HH.i
            [ HS.class_ "fas fa-ellipsis-h options-button-icon"
            , HE.onClick $ togglePopover shownId
            ]
            []
        ]
        (guard (input.status == SignedInSelf)
        [ popoverItem
            (const $ Hooks.raise outputToken Edit)
            [ HH.text "Edit profile" ]
        ]
        <>
        [ popoverItem
            (const $ lift openProfileInNewTab)
            [ HH.text "Open profile in new tab" ]
        , popoverItem
            (const $ lift copyProfileAddress)
            [ HH.text "Copy profile address" ]
        ]
        <>
        guard (input.status == SignedInSelf)
        [ HH.div
            [ HS.class_ "popover-item"
            , HE.onClick $ const $ Hooks.raise outputToken Delete
            ]
            [ HH.span [ HS.class_ "delete-account-option" ] [ HH.text $ "Delete profile" ] ]
        ])

playerProfileOptions :: ∀ action slots left.
    Input -> (Output -> action) -> HH.ComponentHTML action (playerProfileOptions :: Slot_OI Output String | slots) (Async left)
playerProfileOptions input handleOutput =
    HH.slot (Proxy :: _ "playerProfileOptions") input.handle component input handleOutput
