module Perun.Request.Body
    ( Body
    , fromRequest
    , readBuffers
    , collectBuffers
    , readAsUtf8
    ) where

import Prelude

import Effect (Effect)
import Node.Buffer (Buffer, concat_, toString___)
import Node.Events.EventEmitter (class EventEmitter)
import Node.Http.Server.Request (Request)
import Node.Stream.Readable (class Readable)
import Node.Stream.Readable.Events (collectDataEvents, readDataEvents)
import Unsafe.Coerce (unsafeCoerce)

newtype Body = Body Request

derive newtype instance eventEmitterBody :: EventEmitter Body

derive newtype instance readableBody :: Readable Body

fromRequest :: Request -> Body
fromRequest = Body

readBuffers :: (Buffer -> Effect Unit) -> Effect Unit -> Body -> Effect Body
readBuffers dataListener endListener body =
    body # readDataEvents (unsafeCoerce >>> dataListener) endListener

collectBuffers :: (Buffer -> Effect Unit) -> Body -> Effect Body
collectBuffers callback body =
    body # collectDataEvents (map unsafeCoerce >>> concat_ >=> callback)

readAsUtf8 :: (String -> Effect Unit) -> Body -> Effect Body
readAsUtf8 callback body = body # collectBuffers (toString___ >=> callback)
