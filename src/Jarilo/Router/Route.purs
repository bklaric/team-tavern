module Jarilo.Router.Route where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Data.Variant (class VariantMatchCases, Variant, inj)
import Jarilo.Route (FullRoute, Route)
import Jarilo.Router.Method (class MethodRouter, MethodError, methodRouter)
import Jarilo.Router.Path (class PathRouter, PathError, pathRouter')
import Jarilo.Router.Query (class QueryRouter, QueryError, queryRouter')
import Jarilo.Serve.SendResponse.Response (class SendResponse, sendResponse, sendResponse')
import Perun.Request as Perun
import Perun.Response as PerunRes
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record.Builder (build)
import Type.Proxy (Proxy(..))

type RouteError = Variant
    ( methodError :: MethodError
    , pathError :: PathError
    , queryError :: QueryError
    )

type RouteResult pathParams queryParams body responses =
    { path :: Record pathParams
    , query :: Record queryParams
    , body :: body
    , respond :: Variant responses -> PerunRes.Response
    }

class RouteRouter (route :: Route) pathParams queryParams body responses | route -> pathParams queryParams body responses where
    routeRouter
        :: Proxy route
        -> Perun.Request
        -> Either RouteError (RouteResult pathParams queryParams body responses)

instance
    ( MethodRouter method body
    , PathRouter path () pathParams
    , QueryRouter query () queryParams
    , RowToList responseHandlerRow responseHandlerRowList
    , VariantMatchCases responseHandlerRowList wtf PerunRes.Response
    , Union wtf () responseRow
    , SendResponse responseKind () responseHandlerRow
    ) =>
    RouteRouter (FullRoute method path query responseKind) pathParams queryParams body responseRow where
    routeRouter _ { method, path, query, body: body' } = let
        methodProxy = (Proxy :: _ method)
        pathProxy = (Proxy :: _ path)
        queryProxy = (Proxy :: _ query)
        in do
        body <- methodRouter methodProxy method body' # lmap (inj (Proxy :: _ "methodError"))
        pathBuilder <- pathRouter' pathProxy path # lmap (inj (Proxy :: _ "pathError"))
        queryBuilder <- queryRouter' queryProxy query # lmap (inj (Proxy :: _ "queryError"))
        pure
            { path: build pathBuilder {}
            , query: build queryBuilder {}
            , body
            , respond: sendResponse' (Proxy :: _ responseKind)
            }
