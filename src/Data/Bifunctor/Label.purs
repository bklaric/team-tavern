module Data.Bifunctor.Label where

import Prelude

import Data.Bifunctor (class Bifunctor, lmap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj, on)
import Prim.Row (class Cons)

label
    :: forall bifunctor label left lefts' lefts right
    .  Bifunctor bifunctor
    => Cons label left lefts' lefts
    => IsSymbol label
    => SProxy label
    -> bifunctor left right
    -> bifunctor (Variant lefts) right
label label' = lmap (inj label')

labelMap
    :: forall bifunctor label leftIn leftOut lefts' lefts right
    .  Bifunctor bifunctor
    => Cons label leftOut lefts' lefts
    => IsSymbol label
    => SProxy label
    -> (leftIn -> leftOut)
    -> bifunctor leftIn right
    -> bifunctor (Variant lefts) right
labelMap label' mapper = lmap (mapper >>> inj label')

relabel
    :: forall bifunctor fromLabel toLabel value leftsIn lefts leftsOut right
    .  Bifunctor bifunctor
    => Cons fromLabel value leftsOut leftsIn
    => Cons toLabel value lefts leftsOut
    => IsSymbol fromLabel
    => IsSymbol toLabel
    => SProxy fromLabel
    -> SProxy toLabel
    -> bifunctor (Variant leftsIn) right
    -> bifunctor (Variant leftsOut) right
relabel fromLabel toLabel = lmap (on fromLabel (inj toLabel) identity)

relabelMap
    :: forall bifunctor fromLabel toLabel leftIn leftOut
       leftsIn lefts leftsOut right
    .  Bifunctor bifunctor
    => Cons fromLabel leftIn leftsOut leftsIn
    => Cons toLabel leftOut lefts leftsOut
    => IsSymbol fromLabel
    => IsSymbol toLabel
    => SProxy fromLabel
    -> SProxy toLabel
    -> (leftIn -> leftOut)
    -> bifunctor (Variant leftsIn) right
    -> bifunctor (Variant leftsOut) right
relabelMap fromLabel toLabel mapper =
    lmap (on fromLabel (mapper >>> inj toLabel) identity)
