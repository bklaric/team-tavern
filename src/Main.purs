module Main where

import Prelude

import Control.Monad.Effect.Ref (Ref, modifyRef, newRef, readRef)
import Control.Monad.Except (runExcept)
import Data.Array (cons)
import Data.Either (Either, either)
import Data.Foreign (Foreign, ForeignError, unsafeFromForeign, unsafeReadTagged)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Buffer (Buffer, concat_, toString___)
import Node.Events.EventEmitter (class EventEmitter, on')
import Node.Http.Server (Request, Response, create)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Readable (class Readable)
import Node.Stream.Readable.Events (data', end)
import Node.Stream.Writable (endBuffer_, endString__)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

foreignToBuffer :: Foreign -> Either (NonEmptyList ForeignError) Buffer
foreignToBuffer foreignBuffer =
    unsafeReadTagged "Uint8Array" foreignBuffer
    # runExcept
    <#> unsafeFromForeign

readEventsUtf8 :: forall readable. Readable readable =>
    readable -> (String -> Effect Unit) -> Effect Unit
readEventsUtf8 request callback = do
    body <- newRef ([] :: Array Buffer)
    _ <- request # on' data' \foreignData ->
        foreignToBuffer foreignData # either (const $ pure unit) \buffer ->
            modifyRef body (cons buffer)
    _ <- request # on' end
        (readRef body >>= concat_ >>= toString___ >>= callback)
    pure unit

requestHandler :: Request -> Response -> Effect Unit
requestHandler request response = do
    response # endString__ "Hello world!" # void

main :: Effect Unit
main = do
    server <- create requestHandler
    server # listen_ listenOptions
