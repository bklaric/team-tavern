module TeamTavern.Client.Script.Navigate
    ( navigate
    , navigate_
    , navigateWithEvent
    , navigateWithEvent_
    , navigated
    , replaceState
    , navigateReplace
    , navigateReplace_
    , hardNavigate
    ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (setTimeout)
import Foreign (Foreign)
import Web.Event.CustomEvent as CustomEvent
import Web.Event.Event (EventType(..), defaultPrevented, preventDefault)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.History as History
import Web.HTML.Location (href, pathname, search, setHref)
import Web.HTML.Window (history, location)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent, altKey, button, ctrlKey, metaKey, shiftKey, toEvent)
import Yoga.JSON (class WriteForeign, write)

-- | Fired on the window once the location has changed for a page of the site,
-- | for the router to show it. Going back and forward fires `popstate` instead.
navigated :: EventType
navigated = EventType "teamtavern:navigated"

-- The router hears of the change on a later tick, once whatever called this is
-- done: it re-renders as soon as it hears, and doing that inside a page's own
-- handler or initializer pulls the page out from under it. The history entry
-- is written at once, so going back in between leaves it intact. Going back or
-- forward in between routes the page it reaches on `popstate`, and the
-- navigation it overtook is dropped rather than routed again over that page.
type WriteHistory = Foreign -> DocumentTitle -> URL -> History.History -> Effect Unit

changeLocation :: WriteHistory -> Foreign -> String -> Effect Unit -> Effect Unit
changeLocation writeHistory state path after = do
    window >>= history >>= writeHistory state (DocumentTitle path) (URL path)
    written <- window >>= location >>= href
    event <- CustomEvent.new navigated
    void $ setTimeout 0 do
        current <- window >>= location >>= href
        when (current == written) do
            window <#> Window.toEventTarget >>= dispatchEvent (CustomEvent.toEvent event) # void
            after

-- A link to the page already open adds no entry to go back through.
navigate :: ∀ effect state. MonadEffect effect =>
    WriteForeign state => state -> String -> effect Unit
navigate state path = liftEffect do
    location' <- window >>= location
    current <- (<>) <$> pathname location' <*> search location'
    let writeHistory = if path == current then History.replaceState else pushState
    changeLocation writeHistory (write state) path (window >>= Window.scroll 0 0)

navigate_ :: ∀ effect. MonadEffect effect => String -> effect Unit
navigate_ path = navigate {} path

navigateWithEvent :: ∀ effect state. MonadEffect effect => WriteForeign state =>
    state -> String -> MouseEvent -> effect Unit
navigateWithEvent state path event = liftEffect do
    handled <- defaultPrevented $ toEvent event
    -- A click with a modifier opens the link in another tab or window, which
    -- the browser does itself from the link's href.
    let modified = ctrlKey event || metaKey event || shiftKey event || altKey event
    unless (handled || modified || button event /= 0) do
        preventDefault $ toEvent event
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
navigateReplace state path =
    changeLocation History.replaceState (write state) path (pure unit) # liftEffect

navigateReplace_ :: ∀ effect. MonadEffect effect => String -> effect Unit
navigateReplace_ path = navigateReplace {} path

hardNavigate :: ∀ effect. MonadEffect effect => String -> effect Unit
hardNavigate path = window >>= location >>= setHref path # liftEffect
