module TeamTavern.Client.Pages.Profiles.PlayerProfileOptions (playerProfileOptions) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location, open)

type Input = { nickname :: String, handle :: String }

profileUrl :: Input -> Effect String
profileUrl { nickname, handle } = do
    origin' <- window >>= location >>= origin
    pure $ origin' <> "/players/" <> nickname <> "/profiles/" <> handle

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component $ \_ input -> Hooks.do
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
        [ popoverItem
            (const $ lift openProfileInNewTab)
            [ HH.text "Open profile in new tab" ]
        , popoverItem
            (const $ lift copyProfileAddress)
            [ HH.text "Copy profile address" ]
        ]

playerProfileOptions :: ∀ action slots left.
    Input -> HH.ComponentHTML action (playerProfileOptions :: Slot__String | slots) (Async left)
playerProfileOptions input =
    HH.slot (Proxy :: _ "playerProfileOptions") input.nickname component input absurd
