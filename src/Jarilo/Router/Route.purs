module Jarilo.Router.Route where

import Prelude

import Async (Async)
import Data.Variant (class VariantMatchCases, Variant)
import Jarilo.Router.Request (class RequestRouter, RequestError, RequestResult, requestRouter)
import Jarilo.Router.Response (class ResponseRouter, responseRouter)
import Jarilo.Types (FullRoute, Route)
import Perun.Request as Perun
import Perun.Response as PerunRes
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

data RouteResult pathParams queryParams realBody responses = RouteResult
    (RequestResult pathParams queryParams realBody)
    (Variant responses -> PerunRes.Response)

class RouteRouter (route :: Route) pathParams queryParams realBody responses | route -> pathParams queryParams realBody responses where
    routeRouter
        :: Proxy route
        -> Perun.Request
        -> Async RequestError (RouteResult pathParams queryParams realBody responses)

instance
    ( RequestRouter request pathParams queryParams body
    , ResponseRouter response () responseHandlerRow
    , RowToList responseHandlerRow responseHandlerRowList
    , VariantMatchCases responseHandlerRowList wtf PerunRes.Response
    , Union wtf () responseRow
    ) =>
    RouteRouter (FullRoute request response) pathParams queryParams body responseRow where
    routeRouter _ request = do
        requestResult <- requestRouter (Proxy :: _ request) request
        let respond = responseRouter (Proxy :: _ response)
        pure $ RouteResult requestResult respond
