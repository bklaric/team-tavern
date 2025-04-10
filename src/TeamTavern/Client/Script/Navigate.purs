module TeamTavern.Client.Script.Navigate
    ( navigate
    , navigate_
    , navigateWithEvent
    , navigateWithEvent_
    , replaceState
    , navigateReplace
    , navigateReplace_
    , hardNavigate
    ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (setTimeout)
import TeamTavern.Client.Script.PopStateEvent as PopStateEvent
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML as Html
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.History as History
import Web.HTML.Location (setHref)
import Web.HTML.Window (history, location)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Yoga.JSON (class WriteForeign, write)

navigate :: ∀ effect state. MonadEffect effect =>
    WriteForeign state => state -> String -> effect Unit
navigate state path = liftEffect do
    window
        >>= history
        >>= pushState (write state) (DocumentTitle path) (URL path)
    popStateEvent <-
        PopStateEvent.create (write state) <#> PopStateEvent.toEvent
    void $ setTimeout 0 do
        window <#> Window.toEventTarget >>= dispatchEvent popStateEvent # void
        Html.window >>= Window.scroll 0 0

navigate_ :: ∀ effect. MonadEffect effect => String -> effect Unit
navigate_ path = navigate {} path

navigateWithEvent :: ∀ effect state. MonadEffect effect => WriteForeign state =>
    state -> String -> MouseEvent -> effect Unit
navigateWithEvent state path event = do
    preventMouseDefault event
    navigate state path

navigateWithEvent_ :: ∀ effect. MonadEffect effect => String -> MouseEvent -> effect Unit
navigateWithEvent_ path event = navigateWithEvent {} path event

replaceState :: forall state effect. WriteForeign state => MonadEffect effect =>
    state -> String -> effect Unit
replaceState state path  =
    window
    >>= history
    >>= History.replaceState (write state) (DocumentTitle path) (URL path)
    # liftEffect

navigateReplace :: ∀ state effect. MonadEffect effect => WriteForeign state =>
    state -> String -> effect Unit
navigateReplace state path = liftEffect do
    replaceState state path
    popStateEvent <-
        PopStateEvent.create (write state) <#> PopStateEvent.toEvent
    void $ setTimeout 0
        (window <#> Window.toEventTarget >>= dispatchEvent popStateEvent # void)

navigateReplace_ :: ∀ effect. MonadEffect effect => String -> effect Unit
navigateReplace_ path = navigateReplace {} path

hardNavigate :: ∀ effect. MonadEffect effect => String -> effect Unit
hardNavigate path = window >>= location >>= setHref path # liftEffect
