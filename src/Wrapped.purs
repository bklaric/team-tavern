module Wrapped where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import Data.Validation.Semigroup (V, fromEither)

shout :: forall t11 t12. Semigroup t12 => t11 -> Maybe t12 -> V t12 t11
shout valid maybe =
    case maybe of
    Just invalid -> invalid # Left # fromEither
    Nothing -> Right valid # fromEither

create
    :: forall container input canonicalized error result
    .  Traversable container
    => Semigroup (container error)
    => container (canonicalized -> Maybe error)
    -> (input -> canonicalized)
    -> (canonicalized -> result)
    -> input
    -> V (container error) result
create validate canonicalize construct input = let
    canonicalized = canonicalize input
    in
    validate # traverse (_ $ canonicalized) # shout (construct canonicalized)
