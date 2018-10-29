module Data.Validated.Label where

import Prelude

import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Prim.Row (class Cons)
import Data.Validated (Validated, lmap)

label
    :: forall errors errors' left label right
    .  Cons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Validated left right
    -> Validated (NonEmptyList (Variant errors)) right
label label' validated' = validated' # lmap (singleton <<< inj label')
