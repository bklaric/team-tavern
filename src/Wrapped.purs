module Wrapped where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe, maybe)
import Data.Traversable (class Foldable, foldl)

create
    :: forall result input container error canonicalized
    .  Foldable container
    => (input -> canonicalized)
    -> container (canonicalized -> Maybe error)
    -> (canonicalized -> result)
    -> input
    -> Either (List error) result
create canonicalize validate construct input = let
    canonicalized = canonicalize input
    in
    validate
    # foldl
        (\errors validate' ->
            maybe errors (flip Cons errors) (validate' canonicalized))
        Nil
    # case _ of
    Nil -> Right $ construct canonicalized
    errors -> Left errors
