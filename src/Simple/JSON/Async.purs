module Simple.JSON.Async where

import Prelude

import Async (Async)
import Async as Async
import Data.List.Types (NonEmptyList)
import Foreign (Foreign, ForeignError)
import Simple.JSON (class ReadForeign)
import Simple.JSON as Json

readJSON :: forall read. ReadForeign read =>
    String -> Async (NonEmptyList ForeignError) read
readJSON = Json.readJSON >>> Async.fromEither

read :: forall read. ReadForeign read =>
    Foreign -> Async (NonEmptyList ForeignError) read
read = Json.read >>> Async.fromEither
