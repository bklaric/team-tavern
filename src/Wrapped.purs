module Wrapped where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Foldable, foldl)

create
    :: ∀ result input container error canonicalized
    .  Foldable container
    => (input -> canonicalized)
    -> container (canonicalized -> Maybe error)
    -> (canonicalized -> result)
    -> input
    -> Either (NonEmptyArray error) result
create canonicalize validate construct input = let
    canonicalized = canonicalize input
    in
    validate
    # foldl
        (\errors validate' ->
            maybe errors (Array.snoc errors) (validate' canonicalized))
        []
    # Array.uncons
    # case _ of
    Nothing -> Right $ construct canonicalized
    Just { head, tail } -> Left $ head `cons'` tail
