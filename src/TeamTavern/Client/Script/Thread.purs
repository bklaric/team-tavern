module TeamTavern.Client.Script.Thread (autosize, isWide, scrollThreadsToEnd) where

import Prelude

import Effect (Effect)
import Web.HTML (HTMLElement)

-- | Grows a text area to fit what is written in it, as far as its CSS
-- | `max-height` lets it, and shrinks it back when it is emptied.
foreign import autosize :: HTMLElement -> Effect Unit

-- | Scrolls every thread on the page to its latest message, once the page has
-- | drawn it.
foreign import scrollThreadsToEnd :: Effect Unit

-- | Whether the window is wide enough for the inbox to show a conversation
-- | beside the list.
foreign import isWide :: Effect Boolean
