module TeamTavern.Client.Shared.ClientError (reportClientError) where

import Prelude

import Async (runAsync)
import Async.Promisey (promiseToAsync)
import Data.Maybe (Maybe)
import Effect (Effect)
import Jarilo as Jarilo
import TeamTavern.Client.Script.ClientError (bundleName)
import TeamTavern.Routes.ClientError.ReportClientError (ReportClientError)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window

-- | Tells the server of a failure the page didn't expect, the request's method
-- | and path if a request failed, without waiting on the answer. A report
-- | that fails is dropped, never reported.
reportClientError :: Maybe String -> String -> Effect Unit
reportClientError request failure = do
    page <- window >>= Window.location >>= Location.pathname
    Jarilo.fetch (Proxy :: _ ReportClientError) {} {} { page, request, failure, bundle: bundleName } "/api" {}
        # promiseToAsync
        # runAsync (const $ pure unit)
