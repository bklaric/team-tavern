module Wrapped.Validated where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Validated (Validated, fromEither)
import Wrapped as Wrapped

create
    :: ∀ result input container error canonicalized
    .  Foldable container
    => (input -> canonicalized)
    -> container (canonicalized -> Maybe error)
    -> (canonicalized -> result)
    -> input
    -> Validated (NonEmptyArray error) result
create canonicalize validate construct input =
    Wrapped.create canonicalize validate construct input # fromEither
