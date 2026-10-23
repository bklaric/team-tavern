module TeamTavern.Client.Script.QueryParams (forgetLinkSecrets, getQueryParam, openedQuery, removeQueryParam) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- A parameter that holds a secret, such as a reset nonce or a provider's code,
-- is read here though `index.html` took it out of the address, as long as its
-- page is open.

foreign import getQueryParamImpl :: String -> Effect (Nullable String)

getQueryParam :: ∀ monad. MonadEffect monad => String -> monad (Maybe String)
getQueryParam param = getQueryParamImpl param <#> toMaybe # liftEffect

foreign import removeQueryParamImpl :: String -> Effect Unit

-- | Takes the parameter out of the address without a history entry of its own,
-- | keeping the entry's state.
removeQueryParam :: ∀ monad. MonadEffect monad => String -> monad Unit
removeQueryParam param = removeQueryParamImpl param # liftEffect

-- | The query the page was opened with, raw: the address's, and the secrets
-- | `index.html` took out of it.
foreign import openedQuery :: Effect String

-- | Drops the secrets `index.html` took out of the address, once they have
-- | been used, so a reload doesn't use them again.
foreign import forgetLinkSecrets :: Effect Unit
