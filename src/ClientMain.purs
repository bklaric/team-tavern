module ClientMain where

import Prelude

import Async (Async, runAsync)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, makeAff)
import Halogen (hoist, liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import TeamTavern.Client.Main as Main
import Web.Event.EventTarget (addEventListener)
import Web.Event.EventTarget as DOM
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent.EventTypes as PSET
import Web.HTML.Location as Location
import Web.HTML.Window as Window

asyncToAff
    :: forall left right
    .  (left -> Error)
    -> Async left right
    -> Aff right
asyncToAff toError async = async # lmap toError # flip runAsync # (\cont ->
    \callback -> cont callback <#> mempty) # makeAff

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    { query } <- runUI (hoist (asyncToAff absurd) Main.main) unit body
    listener <- liftEffect $ DOM.eventListener $ const do
        path <- window >>= Window.location >>= Location.pathname
        query (Main.ChangeRoute path unit) # launchAff_
    window
        <#> Window.toEventTarget
        >>= addEventListener PSET.popstate listener false
        # liftEffect
