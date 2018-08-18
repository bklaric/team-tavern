module TeamTavern.Client.Script.Navigate where

import Prelude

import Effect (Effect)
import Foreign (unsafeToForeign)
import TeamTavern.Client.Script.PopStateEvent as PopStateEvent
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.Window (history)
import Web.HTML.Window as Window

navigate :: String -> Effect Unit
navigate path = do
    window
        >>= history
        >>= pushState (unsafeToForeign {}) (DocumentTitle path) (URL path)
    popState <- PopStateEvent.create <#> PopStateEvent.toEvent
    window <#> Window.toEventTarget >>= dispatchEvent popState # void
