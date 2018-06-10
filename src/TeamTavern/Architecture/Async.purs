module TeamTavern.Architecture.Async where

import Prelude

import Async (Async(..), runAsync)
import Control.Monad.Cont (ContT(..))
import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Either (Either(..), either)
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
examineErrorWith logger async = Async $ ExceptT $ ContT $ \callback ->
    runAsync async
        case _ of
        Left error -> do
            logger error
            callback $ Left error
        Right result -> callback $ Right result

examineErrorWith'
    :: forall left right
    .  (left -> (forall voidLeft. Async voidLeft Unit))
    -> Async left right
    -> Async left right
examineErrorWith' logger async = Async $ ExceptT $ ContT $ \callback ->
    runAsync async
        case _ of
        Left error -> do
            runAsync (logger error) (either absurd pure)
            callback $ Left error
        Right result -> callback $ Right result
