module Perun.Response where

import Prelude

import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import MultiMap (MultiMap, toUnfoldable)
import Node.Http.Server.Response (setHeader', setStatusCode)
import Node.Http.Server.Response as Node
import Node.Stream.Writable (endString__)

type Response =
    { statusCode :: Int
    , headers :: MultiMap String String
    , content :: String
    }

respond :: Node.Response -> Response -> Effect Unit
respond response { statusCode, headers, content } = do
    setStatusCode statusCode response
    foreachE (toUnfoldable headers) \(Tuple header values) ->
        setHeader' header (fromFoldable values) response
    endString__ content response # void
