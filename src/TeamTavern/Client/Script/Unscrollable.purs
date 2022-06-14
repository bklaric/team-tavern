module TeamTavern.Client.Script.Unscrollable where

import Prelude

import Data.Maybe (maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM.Element (setClassName)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

makeWindowUnscrollable :: forall monad. MonadEffect monad => monad Unit
makeWindowUnscrollable =
    window
    >>= document
    >>= body
    >>= (maybe (pure unit) (toElement >>> setClassName "unscrollable"))
    # liftEffect

makeWindowScrollable :: forall monad. MonadEffect monad => monad Unit
makeWindowScrollable =
    window
    >>= document
    >>= body
    >>= (maybe (pure unit) (toElement >>> setClassName ""))
    # liftEffect
