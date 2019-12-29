module TeamTavern.Client.Script.Unscrollable where

import Prelude

import Data.Maybe (maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (setClassName)
import Web.HTML.Window (document)

makeWindowUnscrollable :: forall monad. MonadEffect monad => monad Unit
makeWindowUnscrollable =
    window
    >>= document
    >>= body
    >>= (maybe (pure unit) (setClassName "unscrollable"))
    # liftEffect

makeWindowScrollable :: forall monad. MonadEffect monad => monad Unit
makeWindowScrollable =
    window
    >>= document
    >>= body
    >>= (maybe (pure unit) (setClassName ""))
    # liftEffect
