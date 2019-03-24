module TeamTavern.Client.Script.QueryParams (getQueryParam) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

foreign import getQueryParamImpl :: String -> Effect (Nullable String)

getQueryParam :: String -> Effect (Maybe String)
getQueryParam param = getQueryParamImpl param <#> toMaybe
