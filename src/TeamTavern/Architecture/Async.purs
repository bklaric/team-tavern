module TeamTavern.Architecture.Async where

import Prelude

import Async (Async(..), fromEither)
import Control.Monad.Except (withExceptT)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Prim.Row (class Cons)
import Validated (Validated, toEither)

label
    :: forall errors errors' left label right
    .  Cons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Async left right
    -> Async (Variant errors) right
label label' (Async exceptT) =
    exceptT # withExceptT (inj label') # Async

fromValidated :: forall left right. Validated left right -> Async left right
fromValidated = toEither >>> fromEither
