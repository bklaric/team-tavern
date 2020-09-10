module AsyncV where

import Prelude

import Async (Async(..))
import Control.Monad.Cont (class MonadTrans, ContT(..), lift)
import Control.Monad.Except (ExceptT(..))
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Validated (Validated, validated)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant, inj)
import Effect (Effect)
import Prim.Row (class Cons)
import Undefined (undefined)

newtype ValidatedT invalid monad valid = ValidatedT (monad (Validated invalid valid))

derive instance newtypeValidatedT :: Newtype (ValidatedT left monad right) _

derive instance functorValidatedT :: (Functor monad, Semigroup invalid) => Functor (ValidatedT invalid monad)

instance applyValidatedT :: (Semigroup invalid, Monad monad) => Apply (ValidatedT invalid monad) where
    apply (ValidatedT funMonad) (ValidatedT valueMonad) = ValidatedT do
        fun <- funMonad
        value <- valueMonad
        pure $ fun <*> value

instance applicativeValidatedT :: (Semigroup invalid, Monad monad) => Applicative (ValidatedT invalid monad) where
    pure = ValidatedT <<< pure <<< Validated.valid

instance bindValidatedT :: (Semigroup invalid, Monad monad) => Bind (ValidatedT invalid monad) where
    bind (ValidatedT valueMonad) funV =
        ValidatedT (valueMonad >>= Validated.validated (pure <<< Validated.invalid) (\value -> funV value # unwrap))

instance monadTransValidatedT :: Semigroup invalid => MonadTrans (ValidatedT invalid) where
    lift value = ValidatedT $ Validated.valid <$> value

newtype AsyncV invalid valid = AsyncV (ValidatedT invalid (ContT Unit Effect) valid)

runAsyncV :: forall left right.
    (Validated left right -> Effect Unit) -> AsyncV left right -> Effect Unit
runAsyncV callback = \(AsyncV (ValidatedT (ContT cont))) -> cont callback

runSafeAsyncV :: forall right.
    (right -> Effect Unit) -> (forall left. AsyncV left right) -> Effect Unit
runSafeAsyncV callback = runAsyncV $ validated absurd callback

fromValidated :: forall invalid valid. Validated invalid valid -> AsyncV invalid valid
fromValidated = pure >>> ValidatedT >>> AsyncV

fromEffect :: forall left right. Semigroup left => Effect right -> AsyncV left right
fromEffect = lift >>> map Validated.valid >>> ValidatedT >>> AsyncV

toAsync :: forall invalid valid. AsyncV invalid valid -> Async invalid valid
toAsync (AsyncV (ValidatedT (ContT cont))) = Async (ExceptT (ContT \callback ->
    cont \value -> value # Validated.toEither # callback))

valid :: forall valid invalid. Semigroup invalid => valid -> AsyncV invalid valid
valid = fromValidated <<< Validated.valid

invalid :: forall valid invalid. Semigroup invalid => invalid -> AsyncV invalid valid
invalid = fromValidated <<< Validated.invalid

bimap
    :: forall oldValid oldInvalid newValid newInvalid
    .  Semigroup newInvalid
    => (oldInvalid -> newInvalid)
    -> (oldValid -> newValid)
    -> AsyncV oldInvalid oldValid
    -> AsyncV newInvalid newValid
bimap invalidFunction validFunction (AsyncV (ValidatedT (ContT cont))) =
    AsyncV $ ValidatedT $ ContT \callback ->
        cont \value -> Validated.bimap invalidFunction validFunction value # callback

lmap
    :: forall newInvalid valid oldInvalid
    .  Semigroup newInvalid
    => (oldInvalid -> newInvalid)
    -> AsyncV oldInvalid valid
    -> AsyncV newInvalid valid
lmap invalidFunction asyncV = bimap invalidFunction identity asyncV

rmap
    :: forall invalid newValid oldValid
    .  Semigroup invalid
    => (oldValid -> newValid)
    -> AsyncV invalid oldValid
    -> AsyncV invalid newValid
rmap validFunction asyncV = bimap identity validFunction asyncV

label
    :: forall errors errors' left label right
    .  Cons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> AsyncV left right
    -> AsyncV (NonEmptyList (Variant errors)) right
label label' = lmap (singleton <<< inj label')

labelMap
    :: forall label leftIn leftOut lefts' lefts right
    .  Cons label leftOut lefts' lefts
    => IsSymbol label
    => SProxy label
    -> (leftIn -> leftOut)
    -> AsyncV leftIn right
    -> AsyncV (NonEmptyList (Variant lefts)) right
labelMap label' mapper = lmap (singleton <<< inj label' <<< mapper)

derive instance newtypeAsyncV :: Newtype (AsyncV left right) _

derive newtype instance functorAsyncV :: Semigroup left => Functor (AsyncV left)

-- Check out Apply instance for ContT to understand how this works, because I
-- sure as hell can't explain it.
instance applyAsyncV :: Semigroup invalid => Apply (AsyncV invalid) where
    apply (AsyncV (ValidatedT (ContT funCont))) (AsyncV (ValidatedT (ContT valueCont))) =
        AsyncV $ ValidatedT $ ContT \callback ->
            funCont \funCallback ->
                valueCont \valueCallback ->
                    callback (funCallback <*> valueCallback)

instance applicativeAsyncV :: Semigroup invalid => Applicative (AsyncV invalid) where
    pure = valid

instance bindAsyncV :: Semigroup invalid => Bind (AsyncV invalid) where
    bind (AsyncV valueTrans) funTrans = AsyncV do
        value <- valueTrans
        funTrans value # unwrap

instance monadAsyncV :: Semigroup invalid => Monad (AsyncV invalid)
