module TeamTavern.Client.Script.Analytics where

import Prelude

import Effect (Effect)

foreign import registerPageView :: Effect Unit

foreign import sendEvent :: String -> String -> String -> Effect Unit
