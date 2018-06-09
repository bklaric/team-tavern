module TeamTavern.Architecture.Async where

import Prelude

import Async (Async(..), fromEffect, runAsync)
import Control.Monad.Except (withExceptT)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Effect (Effect)

label
    :: forall errors errors' left label right
    .  RowCons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Async left right
    -> Async (Variant errors) right
label label' (Async exceptT) =
    exceptT # withExceptT (inj label') # Async

examineErrorWith :: forall left right.
    (left -> Effect Unit) -> Async left right -> Async left right
examineErrorWith logger async = do
    fromEffect $ runAsync async
        case _ of
        Left error -> logger error
        Right _ -> pure unit
    async
