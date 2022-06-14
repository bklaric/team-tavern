module Data.Validated.Label where

import Prelude

import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Symbol (class IsSymbol)
import Data.Validated (Validated, lmap)
import Data.Variant (Variant, inj, on)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

type Variants rows = NonEmptyList (Variant rows)

type ValidatedVariants rows = Validated (NonEmptyList (Variant rows))

label
    :: forall errors errors' left label right
    .  Cons label left errors' errors
    => IsSymbol label
    => Proxy label
    -> Validated left right
    -> ValidatedVariants errors right
label label' = lmap (singleton <<< inj label')

labelMap
    :: forall label leftIn leftOut lefts' lefts right
    .  Cons label leftOut lefts' lefts
    => IsSymbol label
    => Proxy label
    -> (leftIn -> leftOut)
    -> Validated leftIn right
    -> ValidatedVariants lefts right
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
    => Proxy fromLabel
    -> Proxy toLabel
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
    => Proxy fromLabel
    -> Proxy toLabel
    -> (leftIn -> leftOut)
    -> Validated (container (Variant leftsIn)) right
    -> Validated (container (Variant leftsOut)) right
relabelMap fromLabel toLabel mapper =
    lmap (map (on fromLabel (mapper >>> inj toLabel) identity))
