module Perun.Server (RequestHandler, create, create_, run, run_) where

import Prelude

import Effect (Effect)
import Node.Errors (Error)
import Node.Http.Server (HttpServer)
import Node.Http.Server (create) as Node
import Node.Server (ListenOptions, listen)
import Perun.Request (Request, readRequest)
import Perun.Response (Response, respond)

type RequestHandler = Request -> (Response -> Effect Unit) -> Effect Unit

create
    :: (Error -> Effect Unit)
    -> (Error -> Effect Unit)
    -> RequestHandler
    -> Effect HttpServer
create requestErrorHandler responseErrorHandler handler =
    Node.create requestErrorHandler responseErrorHandler \request response ->
        readRequest request \request' -> handler request' (respond response)

create_ :: RequestHandler -> Effect HttpServer
create_ handler = create (const mempty) (const mempty) handler

run
    :: ListenOptions
    -> Effect Unit
    -> (Error -> Effect Unit)
    -> (Error -> Effect Unit)
    -> RequestHandler
    -> Effect Unit
run listenOptions onListening onRequestError onResponseError handler = do
    server <- create onRequestError onResponseError handler
    server # listen listenOptions onListening

run_ :: ListenOptions -> RequestHandler -> Effect Unit
run_ listenOptions handler = run listenOptions mempty mempty mempty handler
