module Async where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT(..), lift)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..), either)
import Data.Either (note) as Either
import Data.Either.AlwaysRight (alwaysRight) as Either
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (new, read, write)

newtype Async left right = Async (ExceptT left (ContT Unit Effect) right)

runAsync :: forall left right.
    (Either left right -> Effect Unit) -> Async left right -> Effect Unit
runAsync callback = \(Async (ExceptT (ContT cont))) -> cont callback

runSafeAsync :: forall right.
    (right -> Effect Unit) -> (forall left. Async left right) -> Effect Unit
runSafeAsync callback = runAsync $ either absurd callback

forkAsync :: forall right left.
    (Either left right -> Effect Unit) -> Async left right -> (forall voidLeft. Async voidLeft Unit)
forkAsync callback async = fromEffect $ runAsync callback async

forkSafeAsync :: forall right.
    (right -> Effect Unit) -> (forall voidLeft. Async voidLeft right) -> (forall voidLeft. Async voidLeft Unit)
forkSafeAsync callback = forkAsync $ either absurd callback

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

right :: forall left right. right -> Async left right
right = fromEither <<< Right

left :: forall left right. left -> Async left right
left = fromEither <<< Left

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

unify :: forall right. Async right right -> (forall left. Async left right)
unify = alwaysRight identity identity

attempt
    :: forall left right
    .  Async left right
    -> (forall voidLeft. Async voidLeft (Either left right))
attempt = alwaysRight Left Right

note :: forall right left. left -> Maybe right -> Async left right
note left' maybe = maybe # Either.note left' # fromEither

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
    examineLeftWithAsync (\left' -> fromEffect $ examiner left') async

foreach :: forall left right traversable. Traversable traversable =>
    traversable right -> (right -> Async left Unit) -> Async left Unit
foreach traversable function = void $ traverse function traversable

safeForeach :: forall right traversable. Traversable traversable =>
    traversable right -> (forall left. right -> Async left Unit) -> (forall left. Async left Unit)
safeForeach traversable function = void $ traverse function traversable

derive instance newtypeAsync :: Newtype (Async left right) _

derive newtype instance functorAsync :: Functor (Async left)

instance bifunctorAsync :: Bifunctor Async where
    bimap leftFunction rightFunction (Async exceptT) =
        exceptT # withExceptT leftFunction # map rightFunction # Async

instance applyAsync :: Apply (Async left) where
    apply = ap

instance applicativeAsync :: Applicative (Async left) where
    pure = right

instance bindAsync :: Bind (Async left) where
    bind (Async exceptT) functionA = Async do
        right' <- exceptT
        functionA right' # unwrap

instance monadAsync :: Monad (Async left)

instance monadEffectAsync :: MonadEffect (Async left) where
    liftEffect = fromEffect

newtype ParAsync left right = ParAsync (Async left right)

derive instance newtypeParAsync :: Newtype (ParAsync left right) _

instance functorParAsync :: Functor (ParAsync left) where
    map f = parallel <<< map f <<< sequential

instance applyParAsync :: Apply (ParAsync left) where
    apply (ParAsync leftAsync) (ParAsync rightAsync) =
        ParAsync $ Async $ ExceptT $ ContT \callback -> do
            leftResultRef <- new Nothing
            rightResultRef <- new Nothing

            leftAsync # runAsync \leftResult -> do
                rightResultMaybe <- read rightResultRef
                case rightResultMaybe of
                    Nothing -> write (Just leftResult) leftResultRef
                    Just rightResult -> callback $ leftResult <*> rightResult

            rightAsync # runAsync \rightResult -> do
                leftResultMaybe <- read leftResultRef
                case leftResultMaybe of
                    Nothing -> (write (Just rightResult) rightResultRef)
                    Just leftResult -> callback $ leftResult <*> rightResult

instance applicativeParAsync :: Applicative (ParAsync left) where
    pure = parallel <<< pure

instance altParAsync :: Alt (ParAsync left) where
    alt (ParAsync leftAsync) (ParAsync rightAsync) =
        ParAsync $ Async $ ExceptT $ ContT \callback -> do
            doneRef <- new false

            leftAsync # runAsync \leftResult -> do
                done <- read doneRef
                if done
                    then pure unit
                    else do
                        write true doneRef
                        callback leftResult

            rightAsync # runAsync \rightResult -> do
                done <- read doneRef
                if done
                    then pure unit
                    else do
                        write true doneRef
                        callback rightResult

instance plusParAsync :: Plus (ParAsync left) where
    empty = ParAsync $ Async $ ExceptT $ ContT \_ -> pure unit

instance alternativeParAsync :: Alternative (ParAsync left)

instance monadParParAsync :: Parallel (ParAsync left) (Async left) where
    parallel = ParAsync
    sequential (ParAsync async) = async
