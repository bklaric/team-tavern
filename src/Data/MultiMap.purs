module Data.MultiMap
    ( MultiMap
    , empty
    , singleton
    , singleton'
    , lookup
    , insertOrAppend
    , insertOrAppend'
    , insertOrReplace
    , insertOrReplace'
    , delete
    , values
    , values'
    , fromFoldable
    , toUnfoldable
    , toUnfoldable'
    , toUnfoldable_
    ) where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (List, NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

newtype MultiMap key value = MultiMap (Map key (NonEmptyList value))

derive newtype instance eqMultiMap
    :: (Eq key, Eq value) => Eq (MultiMap key value)

instance eq1MultiMap :: (Eq key) => Eq1 (MultiMap key) where
    eq1 = eq

derive newtype instance ordMultiMap
    :: (Ord key, Ord value) => Ord (MultiMap key value)

instance ord1MultiMap :: (Ord key) => Ord1 (MultiMap key) where
    compare1 = compare

derive newtype instance semigroupMultiMap
    :: (Ord key) => Semigroup (MultiMap key value)

derive newtype instance monoidMultiMap
    :: (Ord key) => Monoid (MultiMap key value)

derive instance functorMultiMap :: Functor (MultiMap key)

derive newtype instance showMultiMap
    :: (Show key, Show value) => Show (MultiMap key value)

instance foldableMultiMap :: Foldable (MultiMap key) where
    foldl   function state map = foldl   function state $ values' map
    foldr   function state map = foldr   function state $ values' map
    foldMap function       map = foldMap function       $ values' map

empty :: forall key value. MultiMap key value
empty = MultiMap Map.empty

singleton :: forall key value. key -> NonEmptyList value -> MultiMap key value
singleton key values'' = MultiMap $ Map.singleton key values''

singleton' :: forall key value. key -> value -> MultiMap key value
singleton' key value = singleton key $ NonEmptyList.singleton value

lookup :: forall key value. Ord key => key -> MultiMap key value -> Maybe (NonEmptyList value)
lookup key (MultiMap map) = Map.lookup key map

insertOrAppend :: forall key value. Ord key =>
    key -> NonEmptyList value -> MultiMap key value -> MultiMap key value
insertOrAppend key values'' (MultiMap map) =
    MultiMap $ Map.alter
        (case _ of
        Nothing -> Just values''
        Just values''' -> Just $ values''' <> values'')
        key
        map

insertOrAppend' :: forall key value. Ord key =>
    key -> value -> MultiMap key value -> MultiMap key value
insertOrAppend' key value multiMap = insertOrAppend key (NonEmptyList.singleton value) multiMap

insertOrReplace :: forall key value. Ord key =>
    key -> NonEmptyList value -> MultiMap key value -> MultiMap key value
insertOrReplace key values'' (MultiMap map) = MultiMap $ Map.insert key values'' map

insertOrReplace' :: forall key value. Ord key =>
    key -> value -> MultiMap key value -> MultiMap key value
insertOrReplace' key value multiMap = insertOrReplace key (NonEmptyList.singleton value) multiMap

delete :: forall key value. Ord key => key -> MultiMap key value -> MultiMap key value
delete key (MultiMap map) = MultiMap $ Map.delete key map

values :: forall key value. MultiMap key value -> List (NonEmptyList value)
values (MultiMap map) = Map.values map

values' :: forall key value. MultiMap key value -> List value
values' = values >=> NonEmptyList.toList

fromFoldable :: forall key value container. Ord key => Foldable container =>
    container (Tuple key (NonEmptyList value)) -> MultiMap key value
fromFoldable = Map.fromFoldable >>> MultiMap

toUnfoldable :: forall key value tuples. Functor tuples => Unfoldable tuples =>
    MultiMap key value -> tuples (Tuple key (NonEmptyList value))
toUnfoldable (MultiMap map) = Map.toUnfoldable map

toUnfoldable' :: forall key value tuples values
    .  Functor tuples => Unfoldable tuples => Unfoldable values
    => MultiMap key value -> tuples (Tuple key (values value))
toUnfoldable' (MultiMap map) =
    Map.toUnfoldable map
    <#> \(Tuple key values'') -> Tuple key (NonEmptyList.toUnfoldable values'')

toUnfoldable_ :: forall value key tuples. Functor tuples => Unfoldable tuples => Bind tuples =>
    MultiMap key value -> tuples (Tuple key value)
toUnfoldable_ (MultiMap map) =
    Map.toUnfoldable map
    <#> (\(Tuple key values'') -> values'' <#> Tuple key # NonEmptyList.toUnfoldable)
    # join
