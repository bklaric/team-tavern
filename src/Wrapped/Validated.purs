module Wrapped.Validated where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Maybe (Maybe)
import Validated (Validated, fromEither)
import Wrapped as Wrapped

create
    :: forall result input container error canonicalized
    .  Foldable container
    => (input -> canonicalized)
    -> container (canonicalized -> Maybe error)
    -> (canonicalized -> result)
    -> input
    -> Validated (List error) result
create canonicalize validate construct input =
    Wrapped.create canonicalize validate construct input # fromEither
