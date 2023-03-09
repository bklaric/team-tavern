module TeamTavern.Client.Main where

import Prelude

import Async.Aff (asyncToAff)
import Control.Bind (bindFlipped)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Halogen (hoist, liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import TeamTavern.Client.Router (Query(..), router)
import TeamTavern.Client.Script.Analytics (identifyNickname, registerSignedIn)
import TeamTavern.Client.Script.Navigate (navigateReplace_)
import TeamTavern.Client.Script.ReloadAds (reloadAds)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener)
import Web.Event.EventTarget as DOM
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent as PSE
import Web.HTML.Event.PopStateEvent.EventTypes as PSET
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window (document)
import Web.HTML.Window as Window

createListener :: ∀ monad. MonadEffect monad => (Event -> Effect Unit) -> monad EventListener
createListener = liftEffect <<< DOM.eventListener

addWindowListener :: ∀ monad. MonadEffect monad => EventType -> EventListener -> monad Unit
addWindowListener event listener =
    window <#> Window.toEventTarget >>= addEventListener event listener false # liftEffect

main :: Effect Unit
main = HA.runHalogenAff do
    _ <- HA.awaitBody
    identifyNickname
    registerSignedIn
    (spa :: _) <- window >>= document <#> toNonElementParentNode >>= getElementById "spa-teamtavern" <#> bindFlipped fromElement <#> unsafePartial fromJust # liftEffect
    state <- window >>= Window.history >>= History.state # liftEffect
    path <- window >>= Window.location >>= Location.pathname # liftEffect
    { query } <- runUI (hoist (asyncToAff absurd) (router state path)) unit spa
    navigationListener <- createListener \event -> do
        let state' = PSE.fromEvent event # unsafePartial fromJust # PSE.state
        path' <- window >>= Window.location >>= Location.pathname
        hostname <- window >>= Window.location >>= Location.hostname
        search' <- window >>= Window.location >>= Location.search
        -- Append ?nntestads=staging in staging so we see test ads there.
        if hostname == "staging.teamtavern.net" && search' == ""
            then navigateReplace_ $ path' <> "?nntestads=staging"
            else query (ChangeRoute state' path' unit) # void # launchAff_
    addWindowListener PSET.popstate navigationListener
    orientationListener <- createListener $ const reloadAds
    addWindowListener (EventType "orientationchange") orientationListener
