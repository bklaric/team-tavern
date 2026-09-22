module TeamTavern.Client.Script.Scroll
    ( focusFirstInvalid
    , onScroll
    , scrollRestorationManual
    , scrollToId
    ) where

import Prelude

import Data.Foldable (for_)
import Data.Int (round)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (fromElement, offsetTop)
import Web.HTML.Window (document, scroll)

-- Scrolls the page so the element sits just below the fixed top bar.
scrollToId :: ∀ monad. MonadEffect monad => String -> monad Unit
scrollToId id = liftEffect do
    element <- window >>= document <#> toNonElementParentNode >>= getElementById id
    for_ (element >>= fromElement) \element' -> do
        top <- offsetTop element'
        window >>= scroll 0 (round top - 41)

foreign import onScrollImpl :: (Number -> Effect Unit) -> Effect (Effect Unit)

-- | Calls back with the page's scroll position as it scrolls, until the
-- | returned effect stops it.
onScroll :: (Number -> Effect Unit) -> Effect (Effect Unit)
onScroll = onScrollImpl

-- | The site puts a page's scroll position back itself, once the page has
-- | drawn what it had: the browser would do it on `popstate`, before anything
-- | is there to scroll to.
foreign import scrollRestorationManual :: Effect Unit

-- | Brings the first field marked invalid into view and focuses its control,
-- | once the page has drawn the marks.
foreign import focusFirstInvalid :: Effect Unit
