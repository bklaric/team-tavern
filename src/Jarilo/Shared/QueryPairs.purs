module Jarilo.Shared.QueryPairs where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Tuple (Tuple, fst)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)

delete :: Key -> QueryPairs Key Value -> QueryPairs Key Value
delete key (QueryPairs pairs) =
    pairs # Array.filter (fst >>> (_ /= key)) # QueryPairs

find :: Key -> QueryPairs Key Value -> Maybe (Tuple Key (Maybe Value))
find key (QueryPairs pairs) = Array.find (fst >>> (_ == key)) pairs

findAll :: Key -> QueryPairs Key Value -> Array (Tuple Key (Maybe Value))
findAll key (QueryPairs pairs) = pairs # Array.filter (fst >>> (_ == key))
