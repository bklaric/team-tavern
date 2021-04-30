module TeamTavern.Client.Script.ReloadAds (reloadAds) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import reloadAdsImpl :: Effect Unit

reloadAds :: forall monad. MonadEffect monad => monad Unit
reloadAds = liftEffect reloadAdsImpl
