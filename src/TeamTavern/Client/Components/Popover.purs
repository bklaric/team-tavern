module TeamTavern.Client.Components.Popover where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Halogen (SubscriptionId, lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import TeamTavern.Client.Pages.Team.CreateProfile (createProfile)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Game.ViewAll.SendResponse as ViewAll
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

popoverContainer :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
popoverContainer content = HH.div [ HS.class_ "popover-container" ] content

popoverButtonCaret :: forall slots action. Boolean -> HH.HTML slots action
popoverButtonCaret popoverShown =
    HH.i
    [ HS.class_ $ "popover-button-caret fas fa-caret-" <> if popoverShown then "up" else "down" ]
    []

popoverBody :: forall slots action.
    Boolean -> Array (HH.HTML slots action) -> Array (HH.HTML slots action)
popoverBody popoverShown content =
    if popoverShown
    then [ HH.div [ HS.class_ "popover" ] content ]
    else []

popoverItem onClick content =
    HH.div [ HS.class_ "popover-item", HE.onClick $ Just <<< onClick ] content

popover :: forall slots action.
    Boolean -> Array (HH.HTML slots action) -> Array (HH.HTML slots action) -> HH.HTML slots action
popover popoverShown containerContent popoverContent =
    popoverContainer $ containerContent <> popoverBody popoverShown popoverContent

primaryButtonPopover
    :: forall slots action
    .  Boolean
    -> String
    -> String
    -> (MouseEvent -> action)
    -> Array (HH.HTML slots action)
    -> HH.HTML slots action
primaryButtonPopover popoverShown icon text showPopover popoverContent =
    popover
    popoverShown
    [ HH.button
        [ HS.class_ "primary-button"
        , HE.onClick $ Just <<< showPopover
        ]
        [ HH.i [ HS.class_ $ icon <> " button-icon" ] []
        , HH.text text
        , popoverButtonCaret popoverShown
        ]
    ]
    popoverContent

subscribeToWindowClick :: forall output slots action state left.
    action -> H.HalogenM state action slots output (Async left) SubscriptionId
subscribeToWindowClick onClick = do
    window <- liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListenerEventSource
            (E.EventType "click") window \_ -> Just onClick
    H.subscribe $ ES.hoist affToAsync windowEventSource

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

togglePopover shownId mouseEvent = do
    mouseEvent # ME.toEvent # E.stopPropagation # liftEffect
    Hooks.modify_ shownId not
