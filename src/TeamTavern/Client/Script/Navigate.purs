module TeamTavern.Client.Script.Navigate
    ( navigate
    , navigate_
    , navigateWithEvent
    , navigateWithEvent_
    , navigateReplace
    , navigateReplace_
    ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Simple.JSON (class WriteForeign, write)
import TeamTavern.Client.Script.PopStateEvent as PopStateEvent
import Web.Event.Event (preventDefault)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), pushState, replaceState)
import Web.HTML.Window (history)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

foreign import setTimeout :: Effect Unit -> Int -> Effect Unit

navigate :: forall effect state. MonadEffect effect =>
    WriteForeign state => state -> String -> effect Unit
navigate state path = liftEffect do
    window
        >>= history
        >>= pushState (write state) (DocumentTitle path) (URL path)
    popStateEvent <-
        PopStateEvent.create (write state) <#> PopStateEvent.toEvent
    setTimeout
        (window <#> Window.toEventTarget >>= dispatchEvent popStateEvent # void)
        0

navigate_ :: forall effect. MonadEffect effect => String -> effect Unit
navigate_ path = navigate {} path

navigateWithEvent :: forall effect state. MonadEffect effect => WriteForeign state =>
    state -> String -> MouseEvent -> effect Unit
navigateWithEvent state path event = do
    liftEffect $ preventDefault $ toEvent event
    navigate state path

navigateWithEvent_ :: forall effect. MonadEffect effect => String -> MouseEvent -> effect Unit
navigateWithEvent_ path event = navigateWithEvent {} path event

navigateReplace :: forall state effect. MonadEffect effect => WriteForeign state =>
    state -> String -> effect Unit
navigateReplace state path = liftEffect do
    window
        >>= history
        >>= replaceState (write state) (DocumentTitle path) (URL path)
    popStateEvent <-
        PopStateEvent.create (write state) <#> PopStateEvent.toEvent
    setTimeout
        (window <#> Window.toEventTarget >>= dispatchEvent popStateEvent # void)
        0

navigateReplace_ :: forall effect. MonadEffect effect => String -> effect Unit
navigateReplace_ path = navigateReplace {} path
