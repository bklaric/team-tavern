module TeamTavern.Client.Script.QueryParams (getQueryParam, removeQueryParam) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import getQueryParamImpl :: String -> Effect (Nullable String)

getQueryParam :: ∀ monad. MonadEffect monad => String -> monad (Maybe String)
getQueryParam param = getQueryParamImpl param <#> toMaybe # liftEffect

foreign import removeQueryParamImpl :: String -> Effect Unit

-- | Takes the parameter out of the address without a history entry of its own,
-- | keeping the entry's state.
removeQueryParam :: ∀ monad. MonadEffect monad => String -> monad Unit
removeQueryParam param = removeQueryParamImpl param # liftEffect
