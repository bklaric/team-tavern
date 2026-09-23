module TeamTavern.Client.Script.Thread (autosize, scrollThreadsToEnd) where

import Prelude

import Effect (Effect)
import Web.HTML (HTMLElement)

-- | Grows a text area to fit what is written in it, as far as its CSS
-- | `max-height` lets it, and shrinks it back when it is emptied.
foreign import autosize :: HTMLElement -> Effect Unit

-- | Scrolls every thread on the page to its latest message, once the page has
-- | drawn it.
foreign import scrollThreadsToEnd :: Effect Unit
