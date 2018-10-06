module Simple.JSON.Async where

import Prelude

import Async (Async)
import Async as Async
import Foreign (Foreign, MultipleErrors)
import Simple.JSON (class ReadForeign)
import Simple.JSON as Json

readJSON :: forall read. ReadForeign read =>
    String -> Async MultipleErrors read
readJSON = Json.readJSON >>> Async.fromEither

read :: forall read. ReadForeign read =>
    Foreign -> Async MultipleErrors read
read = Json.read >>> Async.fromEither
