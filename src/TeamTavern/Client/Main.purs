module TeamTavern.Client.Main where

import Prelude

import Async.Aff (asyncToAff)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (hoist, liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import TeamTavern.Client.Router (Query(..), router)
import Web.Event.EventTarget (addEventListener)
import Web.Event.EventTarget as DOM
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent as PSE
import Web.HTML.Event.PopStateEvent.EventTypes as PSET
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    state <- window >>= Window.history >>= History.state # liftEffect
    path <- window >>= Window.location >>= Location.pathname # liftEffect
    { query } <- runUI (hoist (asyncToAff absurd) (router state path)) unit body
    listener <- liftEffect $ DOM.eventListener \event -> do
        let state' = PSE.fromEvent event # unsafePartial fromJust # PSE.state
        path' <- window >>= Window.location >>= Location.pathname
        query (ChangeRoute state' path' unit) # launchAff_
    window
        <#> Window.toEventTarget
        >>= addEventListener PSET.popstate listener false
        # liftEffect
