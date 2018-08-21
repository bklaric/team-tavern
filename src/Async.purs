module Async where

import Prelude

import Control.Monad.Cont (ContT(..), lift)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..), either)
import Data.Either.AlwaysRight as Either
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Class (class MonadEffect)

newtype Async left right = Async (ExceptT left (ContT Unit Effect) right)

runAsync :: forall left right.
    (Either left right -> Effect Unit) -> Async left right -> Effect Unit
runAsync callback = \(Async (ExceptT (ContT cont))) -> cont callback

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

rightAsync :: forall left right. right -> Async left right
rightAsync = fromEither <<< Right

leftAsync :: forall left right. left -> Async left right
leftAsync = fromEither <<< Left

alwaysRight
    :: forall inLeft inRight outRight
    .  (inLeft -> outRight)
    -> (inRight -> outRight)
    -> Async inLeft inRight
    -> (forall voidLeft. Async voidLeft outRight)
alwaysRight leftFunction rightFunction (Async (ExceptT eitherCont)) =
    eitherCont <#> Either.alwaysRight leftFunction rightFunction
    # ExceptT # Async

alwaysRightWithAsync
    :: forall inLeft inRight outRight
    .  (inLeft -> forall voidLeft. (Async voidLeft outRight))
    -> (inRight -> forall voidLeft. (Async voidLeft outRight))
    -> Async inLeft inRight
    -> (forall voidLeft. Async voidLeft outRight)
alwaysRightWithAsync leftFunction rightFunction async =
    alwaysRight leftFunction rightFunction async # join

alwaysRightWithEffect
    :: forall inLeft inRight outRight
    .  (inLeft -> Effect outRight)
    -> (inRight -> Effect outRight)
    -> Async inLeft inRight
    -> (forall voidLeft. Async voidLeft outRight)
alwaysRightWithEffect leftFunction rightFunction async =
    alwaysRightWithAsync
        (\inLeft -> fromEffect $ leftFunction inLeft)
        (\inRight -> fromEffect $ rightFunction inRight)
        async

attempt
    :: forall left right
    .  Async left right
    -> (forall voidLeft. Async voidLeft (Either left right))
attempt = alwaysRight Left Right

examineLeftWithAsync
    :: forall left right
    .  (left -> (forall voidLeft. Async voidLeft Unit))
    -> Async left right
    -> Async left right
examineLeftWithAsync examiner async = Async $ ExceptT $ ContT $ \callback ->
    async # runAsync
        case _ of
        Left error -> do
            runAsync (either absurd pure) (examiner error)
            callback $ Left error
        Right result -> callback $ Right result

examineLeftWithEffect :: forall left right.
    (left -> Effect Unit) -> Async left right -> Async left right
examineLeftWithEffect examiner async =
    examineLeftWithAsync (\left -> fromEffect $ examiner left) async

derive instance newtypeAsync :: Newtype (Async left right) _

derive newtype instance functorAsync :: Functor (Async left)

instance bifunctorAsync :: Bifunctor Async where
    bimap leftFunction rightFunction (Async exceptT) =
        exceptT # withExceptT leftFunction # map rightFunction # Async

instance applyAsync :: Apply (Async left) where
    apply = ap

instance applicativeAsync :: Applicative (Async left) where
    pure = rightAsync

instance bindAsync :: Bind (Async left) where
    bind (Async exceptT) functionA = Async do
        right <- exceptT
        functionA right # unwrap

instance monadAsync :: Monad (Async left)

instance monadEffectAsync :: MonadEffect (Async left) where
    liftEffect = fromEffect
