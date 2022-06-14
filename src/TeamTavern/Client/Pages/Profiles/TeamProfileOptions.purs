module TeamTavern.Client.Pages.Profiles.TeamProfileOptions (teamProfileOptions) where

import Prelude

import Async (Async, attempt, fromEffect)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Shared.Slot (StringSlot)
import TeamTavern.Client.Snippets.Class as HS
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location, open)

type Input = { teamHandle :: String, gameHandle :: String }

profileUrl :: Input -> Effect String
profileUrl { teamHandle, gameHandle } = do
    origin' <- window >>= location >>= origin
    pure $ origin' <> "/teams/" <> teamHandle <> "/profiles/" <> gameHandle

component :: forall query output left. H.Component query Input output (Async left)
component = Hooks.component $ \_ input -> Hooks.do
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
        ([ HH.i
            [ HS.class_ "fas fa-ellipsis-h options-button-icon"
            , HE.onClick $ togglePopover shownId
            ]
            []
        ])
        [ popoverItem
            (const $ lift openProfileInNewTab)
            [ HH.text "Open profile in new tab" ]
        , popoverItem
            (const $ lift copyProfileAddress)
            [ HH.text "Copy profile address" ]
        ]

teamProfileOptions :: forall action slots left.
    Input -> HH.ComponentHTML action (teamProfileOptions :: StringSlot | slots) (Async left)
teamProfileOptions input =
    HH.slot (Proxy :: _ "teamProfileOptions") input.teamHandle component input absurd
