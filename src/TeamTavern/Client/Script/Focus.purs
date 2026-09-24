module TeamTavern.Client.Script.Focus (focusSoon, focusStill) where

import Prelude

import Effect (Effect)

-- | Focuses the first element the selector matches once the page has drawn,
-- | for a control that the change being drawn puts on the page.
foreign import focusSoon :: String -> Effect Unit

-- | The same, leaving the page scrolled where it is, for a region taking the
-- | focus so that a screen reader reads from it.
foreign import focusStill :: String -> Effect Unit
