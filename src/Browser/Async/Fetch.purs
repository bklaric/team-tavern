module Browser.Async.Fetch (module Browser.Fetch, fetch, fetch_) where

import Prelude

import Async (Async, fromEitherCont)
import Browser.Fetch (FetchOptions, Credentials(..), body, credentials, method)
import Browser.Fetch as F
import Browser.Fetch.Response (Response)
import Data.Options (Options)
import Error (Error)

fetch :: String -> Options F.FetchOptions -> Async Error Response
fetch url options = F.fetch url options # fromEitherCont

fetch_ :: String -> Async Error Response
fetch_ url = F.fetch_ url # fromEitherCont
