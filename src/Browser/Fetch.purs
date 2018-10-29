module Browser.Fetch
    ( FetchOptions
    , Credentials(..)
    , method
    , body
    , credentials
    , fetch
    , fetch_
    ) where

import Prelude

import Browser.Fetch.Response (Response)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Op (Op(..))
import Data.Options (Option, Options(..), opt, options)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Error (Error)
import Foreign (Foreign, unsafeToForeign)

data FetchOptions

data Credentials = Omit | SameOrigin | Include

toString :: Credentials -> String
toString Omit = "omit"
toString SameOrigin = "same-origin"
toString Include = "include"

optCredentials :: forall options. String -> Op (Options options) Credentials
optCredentials key = Op \value ->
    Options [Tuple key (unsafeToForeign $ toString value)]

optShow :: forall value key. Show value => String -> Option key value
optShow key = Op \value -> Options [Tuple key (unsafeToForeign $ show value)]

method :: Option FetchOptions Method
method = optShow "method"

body :: Option FetchOptions String
body = opt "body"

credentials :: Option FetchOptions Credentials
credentials = optCredentials "credentials"

foreign import fetchImpl
    :: String
    -> Foreign
    -> (Response -> Effect Unit)
    -> (Error -> Effect Unit)
    -> Effect Unit

fetch
    :: String
    -> Options FetchOptions
    -> (Either Error Response -> Effect Unit)
    -> Effect Unit
fetch url options' callback =
    fetchImpl url (options options') (Right >>> callback) (Left >>> callback)

fetch_ :: String -> (Either Error Response -> Effect Unit) -> Effect Unit
fetch_ url callback = fetch url (Options []) callback
