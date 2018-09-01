module Async.Validated where

import Prelude

import Async (Async, fromEither)
import Data.Validated (Validated, toEither)

fromValidated :: forall left right. Validated left right -> Async left right
fromValidated = toEither >>> fromEither
