module Data.Validated
    ( Validated
    , valid
    , invalid
    , validated
    , isValid
    , isInvalid
    , toEither
    , fromEither
    , hush
    , note
    , note'
    , bimap
    , lmap
    , rmap
    ) where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable)

data Validated invalid valid = Invalid invalid | Valid valid

valid :: forall invalid valid. Semigroup invalid =>
    valid -> Validated invalid valid
valid valid' = Valid valid'

invalid :: forall invalid valid. Semigroup invalid =>
    invalid -> Validated invalid valid
invalid invalid' = Invalid invalid'

validated
    :: forall invalid valid result
    .  (invalid -> result)
    -> (valid -> result)
    -> Validated invalid valid
    -> result
validated invalidFunction _ (Invalid invalid') = invalidFunction invalid'
validated _ validFunction (Valid valid') = validFunction valid'

isValid :: forall invalid valid. Validated invalid valid -> Boolean
isValid (Valid _) = true
isValid _ = false

isInvalid :: forall invalid valid. Validated invalid valid -> Boolean
isInvalid = not <<< isValid

toEither :: forall invalid valid.
    Validated invalid valid -> Either invalid valid
toEither (Valid valid') = Right valid'
toEither (Invalid invalid') = Left invalid'

fromEither :: forall invalid valid. Semigroup invalid =>
    Either invalid valid -> Validated invalid valid
fromEither (Right valid') = Valid valid'
fromEither (Left invalid') = Invalid invalid'

hush :: forall left right. Validated left right -> Maybe right
hush = validated (const Nothing) Just

note :: forall invalid valid. Semigroup invalid =>
    invalid -> Maybe valid -> Validated invalid valid
note invalid' = maybe (invalid invalid') valid

note'
    :: forall container invalid valid
    .  Semigroup (container invalid)
    => Applicative container
    => invalid
    -> Maybe valid
    -> Validated (container invalid) valid
note' invalid' = maybe (invalid $ pure invalid') valid

bimap
    :: forall oldValid oldInvalid newValid newInvalid
    .  Semigroup newInvalid
    => (oldInvalid -> newInvalid)
    -> (oldValid -> newValid)
    -> Validated oldInvalid oldValid
    -> Validated newInvalid newValid
bimap invalidFunction validFunction validated' =
    case validated' of
    Invalid invalid' -> Invalid $ invalidFunction invalid'
    Valid valid' -> Valid $ validFunction valid'

lmap
    :: forall newInvalid valid oldInvalid
    .  Semigroup newInvalid
    => (oldInvalid -> newInvalid)
    -> Validated oldInvalid valid
    -> Validated newInvalid valid
lmap invalidFunction validated' = bimap invalidFunction identity validated'

rmap
    :: forall invalid newValid oldValid
    .  Semigroup invalid
    => (oldValid -> newValid)
    -> Validated invalid oldValid
    -> Validated invalid newValid
rmap validFunction validated' = bimap identity validFunction validated'

derive instance eqValidated :: (Eq invalid, Eq valid) =>
    Eq (Validated invalid valid)

derive instance ordValidated :: (Ord invalid, Ord valid) =>
    Ord (Validated invalid valid)

instance showValidated :: (Show invalid, Show valid) =>
    Show (Validated invalid valid) where
    show (Valid valid') = "(valid " <> show valid' <> ")"
    show (Invalid invalid') = "(invalid " <> show invalid' <> ")"

instance functorValidated :: Semigroup invalid =>
    Functor (Validated invalid) where
    map = rmap

instance applyValidated :: Semigroup invalid => Apply (Validated invalid) where
    apply (Invalid leftInvalid) (Invalid rightInvalid ) =
        Invalid $ leftInvalid <> rightInvalid
    apply (Invalid invalid') _ = Invalid invalid'
    apply _ (Invalid invalid') = Invalid invalid'
    apply (Valid validFunction) (Valid valid') = Valid $ validFunction valid'

instance applicativeValidated :: Semigroup invalid =>
    Applicative (Validated invalid) where
    pure = Valid

instance bindValidated :: Semigroup invalid => Bind (Validated invalid) where
    bind validated' functionValidated =
        case validated' of
        Valid valid' -> functionValidated valid'
        Invalid invalid' -> Invalid invalid'

instance monadValidated :: Semigroup invalid => Monad (Validated invalid)

instance semigroupValidated :: (Semigroup invalid, Semigroup valid) =>
    Semigroup (Validated invalid valid) where
    append = lift2 append

instance monoidValidated :: (Semigroup invalid, Monoid valid) =>
    Monoid (Validated invalid valid) where
    mempty = pure mempty

instance foldableValidated :: Semigroup invalid => Foldable (Validated invalid) where
    foldMap = validated (const mempty)
    foldr folder default = validated (const default) (flip folder default)
    foldl folder default = validated (const default) (folder default)

instance traversableValidated :: Semigroup invalid => Traversable (Validated invalid) where
    sequence = validated (pure <<< Invalid) (map Valid)
    traverse function = validated (pure <<< Invalid) (map Valid <<< function)
