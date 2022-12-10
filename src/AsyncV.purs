module AsyncV where

import Prelude

import Async (Async(..))
import Control.Monad.Cont (class MonadTrans, ContT(..), lift)
import Control.Monad.Except (ExceptT(..))
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Validated (Validated, validated)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import Effect (Effect)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

newtype ValidatedT invalid monad valid = ValidatedT (monad (Validated invalid valid))

derive instance Newtype (ValidatedT left monad right) _

derive instance (Functor monad, Semigroup invalid) => Functor (ValidatedT invalid monad)

instance (Semigroup invalid, Monad monad) => Apply (ValidatedT invalid monad) where
    apply (ValidatedT funMonad) (ValidatedT valueMonad) = ValidatedT do
        fun <- funMonad
        value <- valueMonad
        pure $ fun <*> value

instance (Semigroup invalid, Monad monad) => Applicative (ValidatedT invalid monad) where
    pure = ValidatedT <<< pure <<< Validated.valid

instance (Semigroup invalid, Monad monad) => Bind (ValidatedT invalid monad) where
    bind (ValidatedT valueMonad) funV =
        ValidatedT (valueMonad >>= Validated.validated (pure <<< Validated.invalid) (\value -> funV value # unwrap))

instance Semigroup invalid => MonadTrans (ValidatedT invalid) where
    lift value = ValidatedT $ Validated.valid <$> value

newtype AsyncV invalid valid = AsyncV (ValidatedT invalid (ContT Unit Effect) valid)

runAsyncV :: ∀ left right.
    (Validated left right -> Effect Unit) -> AsyncV left right -> Effect Unit
runAsyncV callback (AsyncV (ValidatedT (ContT cont))) = cont callback

runSafeAsyncV :: ∀ right.
    (right -> Effect Unit) -> (∀ left. AsyncV left right) -> Effect Unit
runSafeAsyncV callback = runAsyncV $ validated absurd callback

fromValidated :: ∀ invalid valid. Validated invalid valid -> AsyncV invalid valid
fromValidated = pure >>> ValidatedT >>> AsyncV

fromEffect :: ∀ left right. Semigroup left => Effect right -> AsyncV left right
fromEffect = lift >>> map Validated.valid >>> ValidatedT >>> AsyncV

fromAsync :: ∀ invalid valid. Semigroup invalid => Async invalid valid -> AsyncV invalid valid
fromAsync (Async (ExceptT (ContT cont))) = AsyncV (ValidatedT (ContT \callback ->
    cont \value -> value # Validated.fromEither # callback))

toAsync :: ∀ invalid valid. AsyncV invalid valid -> Async invalid valid
toAsync (AsyncV (ValidatedT (ContT cont))) = Async (ExceptT (ContT \callback ->
    cont \value -> value # Validated.toEither # callback))

valid :: ∀ valid invalid. Semigroup invalid => valid -> AsyncV invalid valid
valid = fromValidated <<< Validated.valid

invalid :: ∀ valid invalid. Semigroup invalid => invalid -> AsyncV invalid valid
invalid = fromValidated <<< Validated.invalid

bimap
    :: ∀ oldValid oldInvalid newValid newInvalid
    .  Semigroup newInvalid
    => (oldInvalid -> newInvalid)
    -> (oldValid -> newValid)
    -> AsyncV oldInvalid oldValid
    -> AsyncV newInvalid newValid
bimap invalidFunction validFunction (AsyncV (ValidatedT (ContT cont))) =
    AsyncV $ ValidatedT $ ContT \callback ->
        cont \value -> Validated.bimap invalidFunction validFunction value # callback

lmap
    :: ∀ newInvalid valid oldInvalid
    .  Semigroup newInvalid
    => (oldInvalid -> newInvalid)
    -> AsyncV oldInvalid valid
    -> AsyncV newInvalid valid
lmap invalidFunction asyncV = bimap invalidFunction identity asyncV

rmap
    :: ∀ invalid newValid oldValid
    .  Semigroup invalid
    => (oldValid -> newValid)
    -> AsyncV invalid oldValid
    -> AsyncV invalid newValid
rmap validFunction asyncV = bimap identity validFunction asyncV

label
    :: ∀ errors errors' left label right
    .  Cons label left errors' errors
    => IsSymbol label
    => Proxy label
    -> AsyncV left right
    -> AsyncV (NonEmptyList (Variant errors)) right
label label' = lmap (singleton <<< inj label')

labelMap
    :: ∀ label leftIn leftOut lefts' lefts right
    .  Cons label leftOut lefts' lefts
    => IsSymbol label
    => Proxy label
    -> (leftIn -> leftOut)
    -> AsyncV leftIn right
    -> AsyncV (NonEmptyList (Variant lefts)) right
labelMap label' mapper = lmap (singleton <<< inj label' <<< mapper)

derive instance Newtype (AsyncV left right) _

derive newtype instance Semigroup left => Functor (AsyncV left)

-- Check out Apply instance for ContT to understand how this works, because I
-- sure as hell can't explain it.
instance Semigroup invalid => Apply (AsyncV invalid) where
    apply (AsyncV (ValidatedT (ContT funCont))) (AsyncV (ValidatedT (ContT valueCont))) =
        AsyncV $ ValidatedT $ ContT \callback ->
            funCont \funCallback ->
                valueCont \valueCallback ->
                    callback (funCallback <*> valueCallback)

instance Semigroup invalid => Applicative (AsyncV invalid) where
    pure = valid

instance Semigroup invalid => Bind (AsyncV invalid) where
    bind (AsyncV valueTrans) funTrans = AsyncV do
        value <- valueTrans
        funTrans value # unwrap

instance Semigroup invalid => Monad (AsyncV invalid)
