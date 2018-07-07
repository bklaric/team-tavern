module TeamTavern.Architecture.Either where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy, Variant, inj)

label
    :: forall errors errors' left label right
    .  RowCons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Either left right
    -> Either (Variant errors) right
label label' either' =
    case either' of
    Left left -> Left $ inj label' left
    Right right -> Right right
