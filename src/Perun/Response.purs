module Perun.Response where

import Prelude

import Effect (Effect)
import Node.Http.Server.Response (setStatusCode)
import Node.Http.Server.Response as Node
import Node.Stream.Writable (endString__)

type Response =
    { statusCode :: Int
    , content :: String
    }

respond :: Node.Response -> Response -> Effect Unit
respond response { statusCode, content } = do
    setStatusCode statusCode response
    endString__ content response # void
