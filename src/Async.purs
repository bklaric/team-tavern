module Async where

import Prelude

import Control.Monad.Cont (ContT(..), lift)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)

newtype Async left right = Async (ExceptT left (ContT Unit Effect) right)

runAsync :: forall left right.
    Async left right -> (Either left right -> Effect Unit) -> Effect Unit
runAsync (Async (ExceptT (ContT cont))) = cont

fromEither :: forall left right. Either left right -> Async left right
fromEither = pure >>> ExceptT >>> Async

fromEitherCont :: forall left right.
    ((Either left right -> Effect Unit) -> Effect Unit) -> Async left right
fromEitherCont = ContT >>> ExceptT >>> Async

fromEitherEffect :: forall left right.
    Effect (Either left right) -> Async left right
fromEitherEffect eitherEffect = Async $ ExceptT $ ContT \callback ->
    eitherEffect >>= callback

fromEffect :: forall left right. Effect right -> Async left right
fromEffect = lift >>> map Right >>> ExceptT >>> Async

fromEffectCont :: forall left right.
    ((right -> Effect Unit) -> Effect Unit) -> Async left right
fromEffectCont = ContT >>> map Right >>> ExceptT >>> Async

derive instance newtypeAsync :: Newtype (Async left right) _

derive newtype instance name :: Functor (Async left)

instance bifunctorAsync :: Bifunctor Async where
    bimap leftFunction rightFunction (Async exceptT) =
        exceptT # withExceptT leftFunction # map rightFunction # Async

instance applyAsync :: Apply (Async left) where
    apply = ap

instance applicativeAsync :: Applicative (Async left) where
    pure = fromEither <<< Right

instance bindAsync :: Bind (Async left) where
    bind (Async exceptT) functionA = Async do
        right <- exceptT
        functionA right # unwrap

instance monadAsync :: Monad (Async left)
