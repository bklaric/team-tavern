module Data.Bifunctor.Label where

import Prelude

import Data.Bifunctor (class Bifunctor, lmap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
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
    :: forall bifunctor label liftIn leftOut lefts' lefts right
    .  Bifunctor bifunctor
    => Cons label leftOut lefts' lefts
    => IsSymbol label
    => SProxy label
    -> (liftIn -> leftOut)
    -> bifunctor liftIn right
    -> bifunctor (Variant lefts) right
labelMap label' mapper = lmap (mapper >>> inj label')
