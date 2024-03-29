module Jarilo.Router.Request where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Jarilo.Router.Body (class BodyRouter, BodyError, bodyRouter)
import Jarilo.Router.Method (class MethodRouter, MethodError, methodRouter)
import Jarilo.Router.Path (class PathRouter, PathError, pathRouter)
import Jarilo.Router.Query (class QueryRouter, QueryError, queryRouter)
import Jarilo.Types (FullRequest, Request)
import Perun.Request as Perun
import Type.Proxy (Proxy(..))

data RequestError
    = MethodError MethodError
    | PathError PathError
    | QueryError QueryError
    | BodyError BodyError

type RequestResult pathParams queryParams realBody =
    { path :: Record pathParams
    , query :: Record queryParams
    , headers :: Map String String
    , cookies :: Map String String
    , body :: realBody
    }

class RequestRouter (request :: Request) pathParams queryParams realBody | request -> pathParams queryParams realBody where
    requestRouter
        :: Proxy request
        -> Perun.Request
        -> Async RequestError (RequestResult pathParams queryParams realBody)

instance
    ( MethodRouter method
    , PathRouter path () pathParams
    , QueryRouter query () queryParams
    , BodyRouter body realBody
    ) =>
    RequestRouter (FullRequest method path query body) pathParams queryParams realBody where
    requestRouter _ { method, path, query, headers, cookies, body } = let
        methodProxy = (Proxy :: _ method)
        pathProxy = (Proxy :: _ path)
        queryProxy = (Proxy :: _ query)
        bodyProxy = (Proxy :: _ body)
        in do
        methodRouter methodProxy method # lmap MethodError # fromEither
        pathParams <- pathRouter pathProxy path # lmap PathError # fromEither
        queryParams <- queryRouter queryProxy query # lmap QueryError # fromEither
        realBody <- bodyRouter bodyProxy body # lmap BodyError
        pure
            { path: pathParams
            , query: queryParams
            , headers
            , cookies
            , body: realBody
            }
