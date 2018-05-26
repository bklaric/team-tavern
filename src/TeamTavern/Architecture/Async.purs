module TeamTavern.Architecture.Async where

import Prelude

import Async (Async(..))
import Control.Monad.Except (withExceptT)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)

label
    :: forall errors errors' left label right
    .  RowCons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Async left right
    -> Async (Variant errors) right
label label' (Async exceptT) =
    exceptT # withExceptT (inj label') # Async
