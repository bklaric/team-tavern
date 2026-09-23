module TeamTavern.Client.Script.Focus (focusSoon) where

import Prelude

import Effect (Effect)

-- | Focuses the first element the selector matches once the page has drawn,
-- | for a control that the change being drawn puts on the page.
foreign import focusSoon :: String -> Effect Unit
