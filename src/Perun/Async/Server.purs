module Perun.Async.Server (run, run_) where

import Prelude

import Async (Async, runSafeAsync)
import Effect (Effect)
import Node.Errors (Error)
import Node.Server (ListenOptions)
import Perun.Request (Request)
import Perun.Response (Response)
import Perun.Server (RequestHandler)
import Perun.Server as Perun

fromAsync :: (Request -> (forall left. Async left Response)) -> RequestHandler
fromAsync handler = \request respond ->
    runSafeAsync respond (handler request)

run
    :: ListenOptions
    -> Effect Unit
    -> (Error -> Effect Unit)
    -> (Error -> Effect Unit)
    -> (Request -> (forall left. Async left Response))
    -> Effect Unit
run listenOptions onListening onRequestError onResponseError handler =
    Perun.run
        listenOptions
        onListening
        onRequestError
        onResponseError
        (fromAsync handler)

run_
    :: ListenOptions
    -> (Request -> (forall left. Async left Response))
    -> Effect Unit
run_ listenOptions handler = Perun.run_ listenOptions (fromAsync handler)
