module TeamTavern.Client.Components.Popover where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- HTML

popoverContainer :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
popoverContainer content = HH.div [ HS.class_ "popover-container" ] content

popoverButtonCaret :: forall slots action. Boolean -> HH.HTML slots action
popoverButtonCaret popoverShown =
    HH.i
    [ HS.class_ $ "popover-button-caret fas fa-caret-" <> if popoverShown then "up" else "down" ]
    []

popoverBody :: forall slots action.
    Boolean -> Array (HH.HTML slots action) -> Array (HH.HTML slots action)
popoverBody popoverShown content = guard popoverShown [ HH.div [ HS.class_ "popover popover-bottom-end" ] content ]

popoverItem :: forall slots action.
    (MouseEvent -> action) -> Array (HH.HTML slots action) -> HH.HTML slots action
popoverItem onClick content =
    HH.div [ HS.class_ "popover-item", HE.onClick $ Just <<< onClick ] content

popover :: forall slots action.
    Boolean -> Array (HH.HTML slots action) -> Array (HH.HTML slots action) -> HH.HTML slots action
popover popoverShown containerContent popoverContent =
    popoverContainer $ containerContent <> popoverBody popoverShown popoverContent

-- Hook

type UsePopover state = Hooks.UseEffect (Hooks.UseState Boolean state)

usePopover :: forall left.
    Hooks.Hook (Async left) UsePopover (Tuple Boolean (Hooks.StateId Boolean))
usePopover = Hooks.do
    state @ (Tuple shown shownId) <- Hooks.useState false

    Hooks.useLifecycleEffect do
        window <- window <#> Window.toEventTarget # liftEffect
        let windowEventSource = ES.eventListenerEventSource
                (E.EventType "click") window (const $ Just $ Hooks.put shownId false)
        subscriptionId <- Hooks.subscribe $ ES.hoist affToAsync windowEventSource
        pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.pure state

togglePopover :: forall monad. MonadEffect monad =>
    Hooks.StateId Boolean -> MouseEvent -> Hooks.HookM monad Unit
togglePopover shownId mouseEvent = do
    mouseEvent # ME.toEvent # E.stopPropagation # liftEffect
    Hooks.modify_ shownId not
