module Perun.Response where

import Prelude

import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Data.MultiMap (MultiMap, empty, toUnfoldable)
import Node.Http.Server.Response (setHeader', setStatusCode)
import Node.Http.Server.Response as Node
import Node.Stream.Writable (endString__)

type Response =
    { statusCode :: Int
    , headers :: MultiMap String String
    , body :: String
    }

ok :: MultiMap String String -> String -> Response
ok headers body = { statusCode: 200, headers, body }

ok_ :: String -> Response
ok_ = ok empty

noContent :: MultiMap String String -> Response
noContent headers = { statusCode: 204, headers, body: mempty }

noContent_ :: Response
noContent_ = noContent empty

badRequest :: MultiMap String String -> String -> Response
badRequest headers body = { statusCode: 400, headers, body }

badRequest_ :: String -> Response
badRequest_ = badRequest empty

badRequest__ :: Response
badRequest__ = badRequest_ mempty

unauthorized :: MultiMap String String -> String -> Response
unauthorized headers body = { statusCode: 401, headers, body }

unauthorized_ :: String -> Response
unauthorized_ = unauthorized empty

unauthorized__ :: Response
unauthorized__ = unauthorized_ mempty

forbidden :: MultiMap String String -> String -> Response
forbidden headers body = { statusCode: 403, headers, body }

forbidden_ :: String -> Response
forbidden_ = forbidden empty

forbidden__ :: Response
forbidden__ = forbidden_ mempty

notFound :: MultiMap String String -> String -> Response
notFound headers body = { statusCode: 404, headers, body }

notFound_ :: String -> Response
notFound_ = notFound empty

notFound__ :: Response
notFound__ = notFound_ mempty

internalServerError :: MultiMap String String -> String -> Response
internalServerError headers body = { statusCode: 500, headers, body }

internalServerError_ :: String -> Response
internalServerError_ = internalServerError empty

internalServerError__ :: Response
internalServerError__ = internalServerError_ mempty

respond :: Node.Response -> Response -> Effect Unit
respond response { statusCode, headers, body } = do
    setStatusCode statusCode response
    foreachE (toUnfoldable headers) \(Tuple header values) ->
        setHeader' header (fromFoldable values) response
    endString__ body response # void
