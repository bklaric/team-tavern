module TeamTavern.Client.Script.Url where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

foreign import data Url :: Type

foreign import url :: String -> Effect Url

foreign import href :: Url -> Effect String

foreign import data SearchParams :: Type

foreign import searchParams :: Url -> Effect SearchParams

foreign import append :: String -> String -> SearchParams -> Effect Unit

foreign import set :: String -> String -> SearchParams -> Effect Unit

foreign import getImpl :: String -> SearchParams -> Effect (Nullable String)

get :: String -> SearchParams -> Effect (Maybe String)
get key params = getImpl key params <#> toMaybe

foreign import getAll :: String -> SearchParams -> Effect (Array String)

foreign import delete :: String -> SearchParams -> Effect Unit

foreign import keys :: SearchParams -> Effect (Array String)
