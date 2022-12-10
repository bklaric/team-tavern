module TeamTavern.Client.Pages.Player.PlayerProfileOptions (playerProfileOptions) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Monoid (guard)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Shared.Slot (QuerylessSlot)
import TeamTavern.Client.Snippets.Class as HS
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location, open)

type Input = { status :: Status, nickname :: String, handle :: String }

profileUrl :: Input -> Effect String
profileUrl { nickname, handle } = do
    origin' <- window >>= location >>= origin
    pure $ origin' <> "/players/" <> nickname <> "/profiles/" <> handle

component :: ∀ query left. H.Component query Input Unit (Async left)
component = Hooks.component $ \{ outputToken } input -> Hooks.do
    (Tuple shown shownId) <- usePopover
    let openProfileInNewTab = void $ fromEffect do
            profileUrl' <- profileUrl input
            window >>= open profileUrl' "" ""
    let copyProfileAddress = do
            profileUrl' <- profileUrl input # fromEffect
            writeTextAsync profileUrl' # attempt # void
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
            (const $ Hooks.raise outputToken unit)
            [ HH.text "Edit profile" ]
        ]
        <>
        [ popoverItem
            (const $ lift openProfileInNewTab)
            [ HH.text "Open profile in new tab" ]
        , popoverItem
            (const $ lift copyProfileAddress)
            [ HH.text "Copy profile address" ]
        ])

playerProfileOptions :: ∀ action slots left.
    Input -> action -> HH.ComponentHTML action (playerProfileOptions :: QuerylessSlot Unit String | slots) (Async left)
playerProfileOptions input handleOutput =
    HH.slot (Proxy :: _ "playerProfileOptions") input.handle component input (const handleOutput)
