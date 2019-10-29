module TeamTavern.Client.Script.Title where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)

setWindowTitle :: String -> Effect Unit
setWindowTitle title = window >>= document >>= setTitle title # liftEffect
