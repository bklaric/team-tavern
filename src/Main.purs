module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Buffer (concat_, toString___)
import Node.Http.Server (Request, Response, create)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Readable (class Readable)
import Node.Stream.Readable.Events (collectDataEvents)
import Node.Stream.Writable (endString__)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

-- For server library (because unsafePartial)
readBuffersAsUtf8 :: forall readable. Readable readable =>
    (String -> Effect Unit) -> readable -> Effect readable
readBuffersAsUtf8 callback readable = unsafePartial do
    readable # collectDataEvents
        (map unsafeCoerce
        >>> concat_
        >=> toString___
        >=> callback)

requestHandler :: Request -> Response -> Effect Unit
requestHandler request response = do
    request # readBuffersAsUtf8 (flip endString__ response >>> void) # void

main :: Effect Unit
main = do
    server <- create requestHandler
    server # listen_ listenOptions
