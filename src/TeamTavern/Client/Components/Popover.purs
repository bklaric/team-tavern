module TeamTavern.Client.Components.Popover where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (type (<>))
import Halogen.Query.Event as ES
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- HTML

popoverContainer :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
popoverContainer content = HH.div [ HS.class_ "popover-container" ] content

popoverButtonCaret :: ∀ slots action. Boolean -> HH.HTML slots action
popoverButtonCaret popoverShown =
    HH.i
    [ HS.class_ $ "popover-button-caret fas fa-caret-" <> if popoverShown then "up" else "down" ]
    []

popoverBody :: ∀ slots action.
    Boolean -> Array (HH.HTML slots action) -> Array (HH.HTML slots action)
popoverBody popoverShown content = guard popoverShown [ HH.div [ HS.class_ "popover popover-bottom-end" ] content ]

popoverItem :: ∀ slots action.
    (MouseEvent -> action) -> Array (HH.HTML slots action) -> HH.HTML slots action
popoverItem onClick content =
    HH.div [ HS.class_ "popover-item", HE.onClick onClick ] content

popover :: ∀ slots action.
    Boolean -> Array (HH.HTML slots action) -> Array (HH.HTML slots action) -> HH.HTML slots action
popover popoverShown containerContent popoverContent =
    popoverContainer $ containerContent <> popoverBody popoverShown popoverContent

-- Hook

type UsePopover = Hooks.UseState Boolean <> Hooks.UseEffect <> Hooks.Pure

usePopover :: ∀ left. Hooks.Hook (Async left) UsePopover (Tuple Boolean (Hooks.StateId Boolean))
usePopover = Hooks.do
    state @ (Tuple _ shownId) <- Hooks.useState false

    Hooks.useLifecycleEffect do
        window <- window <#> Window.toEventTarget # liftEffect
        let windowEventSource = ES.eventListener
                (E.EventType "click") window (const $ Just $ Hooks.put shownId false)
        subscriptionId <- Hooks.subscribe windowEventSource
        pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.pure state

stopMouseEventPropagation :: ∀ monad. MonadEffect monad => MouseEvent -> monad Unit
stopMouseEventPropagation = ME.toEvent >>> E.stopPropagation >>> liftEffect

togglePopover :: ∀ monad. MonadEffect monad =>
    Hooks.StateId Boolean -> MouseEvent -> Hooks.HookM monad Unit
togglePopover shownId mouseEvent = do
    stopMouseEventPropagation mouseEvent
    Hooks.modify_ shownId not
