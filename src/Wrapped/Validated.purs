module Wrapped.Validated where

import Prelude

import Data.Foldable (class Foldable)
import Data.List.Types (NonEmptyList)
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
    -> Validated (NonEmptyList error) result
create canonicalize validate construct input =
    Wrapped.create canonicalize validate construct input # fromEither
