module TeamTavern.Client.Script.Navigate where

import Prelude

import Effect (Effect)
import Simple.JSON (class WriteForeign, write)
import TeamTavern.Client.Script.PopStateEvent as PopStateEvent
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.Window (history)
import Web.HTML.Window as Window

navigate :: forall state. WriteForeign state => state -> String -> Effect Unit
navigate state path = do
    window
        >>= history
        >>= pushState (write state) (DocumentTitle path) (URL path)
    popStateEvent <-
        PopStateEvent.create (write state) <#> PopStateEvent.toEvent
    window <#> Window.toEventTarget >>= dispatchEvent popStateEvent # void

navigate_ :: String -> Effect Unit
navigate_ path = navigate {} path
