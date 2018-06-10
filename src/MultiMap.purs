module MultiMap
    ( MultiMap
    , empty
    , singleton
    , singleton'
    , insert
    , toUnfoldable
    , toUnfoldable'
    ) where

import Prelude

import Data.List.NonEmpty (cons)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

newtype MultiMap key value = MultiMap (Map key (NonEmptyList value))

empty :: forall key value. MultiMap key value
empty = MultiMap Map.empty

singleton :: forall key value. key -> value -> MultiMap key value
singleton key value = MultiMap $
    Map.singleton key (NonEmptyList.singleton value)

singleton' :: forall key value. key -> NonEmptyList value -> MultiMap key value
singleton' key values = MultiMap $ Map.singleton key values

insert :: forall key value. Ord key =>
    key -> value -> MultiMap key value -> MultiMap key value
insert key value (MultiMap map) =
    MultiMap $ Map.alter
        (case _ of
        Nothing -> Just $ NonEmptyList.singleton value
        Just values -> Just $ cons value values)
        key
        map

toUnfoldable
    :: forall key value tuples
    .  Functor tuples
    => Unfoldable tuples
    => MultiMap key value
    -> tuples (Tuple key (NonEmptyList value))
toUnfoldable (MultiMap map) = Map.toUnfoldable map

toUnfoldable'
    :: forall key value tuples values
    .  Functor tuples
    => Unfoldable tuples
    => Unfoldable values
    => MultiMap key value
    -> tuples (Tuple key (values value))
toUnfoldable' (MultiMap map) = Map.toUnfoldable map <#> \(Tuple key values) ->
    Tuple key (NonEmptyList.toUnfoldable values)
