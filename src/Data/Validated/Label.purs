module Data.Validated.Label where

import Prelude

import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Validated (Validated, lmap)
import Data.Variant (Variant, inj, on)
import Prim.Row (class Cons)

type VariantNel rows = NonEmptyList (Variant rows)

type VariantValidated rows = Validated (NonEmptyList (Variant rows))

label
    :: forall errors errors' left label right
    .  Cons label left errors' errors
    => IsSymbol label
    => SProxy label
    -> Validated left right
    -> VariantValidated errors right
label label' = lmap (singleton <<< inj label')

labelMap
    :: forall label leftIn leftOut lefts' lefts right
    .  Cons label leftOut lefts' lefts
    => IsSymbol label
    => SProxy label
    -> (leftIn -> leftOut)
    -> Validated leftIn right
    -> VariantValidated lefts right
labelMap label' mapper = lmap (singleton <<< inj label' <<< mapper)

relabel
    :: forall container fromLabel toLabel value leftsIn lefts leftsOut right
    .  Semigroup (container (Variant leftsIn))
    => Semigroup (container (Variant leftsOut))
    => Functor container
    => Cons fromLabel value leftsOut leftsIn
    => Cons toLabel value lefts leftsOut
    => IsSymbol fromLabel
    => IsSymbol toLabel
    => SProxy fromLabel
    -> SProxy toLabel
    -> Validated (container (Variant leftsIn)) right
    -> Validated (container (Variant leftsOut)) right
relabel fromLabel toLabel = lmap (map (on fromLabel (inj toLabel) identity))

relabelMap
    :: forall container fromLabel toLabel leftIn leftOut
       leftsIn lefts leftsOut right
    .  Semigroup (container (Variant leftsIn))
    => Semigroup (container (Variant leftsOut))
    => Functor container
    => Cons fromLabel leftIn leftsOut leftsIn
    => Cons toLabel leftOut lefts leftsOut
    => IsSymbol fromLabel
    => IsSymbol toLabel
    => SProxy fromLabel
    -> SProxy toLabel
    -> (leftIn -> leftOut)
    -> Validated (container (Variant leftsIn)) right
    -> Validated (container (Variant leftsOut)) right
relabelMap fromLabel toLabel mapper =
    lmap (map (on fromLabel (mapper >>> inj toLabel) identity))
