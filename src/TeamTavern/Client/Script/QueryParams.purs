module TeamTavern.Client.Script.QueryParams (getQueryParam) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import getQueryParamImpl :: String -> Effect (Nullable String)

getQueryParam :: forall monad. MonadEffect monad => String -> monad (Maybe String)
getQueryParam param = getQueryParamImpl param <#> toMaybe # liftEffect
