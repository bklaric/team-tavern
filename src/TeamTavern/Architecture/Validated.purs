module TeamTavern.Architecture.Validated where

import Prelude

import Data.List (List, singleton)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Validated (Validated, lmap)

label
    :: forall errors errors' left label right
    .  RowCons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Validated left right
    -> Validated (List (Variant errors)) right
label label' validated' = validated' # lmap (singleton <<< inj label')
