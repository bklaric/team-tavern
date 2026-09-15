module TeamTavern.Client.Script.Scroll (scrollToId) where

import Prelude

import Data.Foldable (for_)
import Data.Int (round)
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
