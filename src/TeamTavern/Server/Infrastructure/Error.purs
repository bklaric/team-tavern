module TeamTavern.Server.Infrastructure.Error where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Semigroup.Foldable (fold1)
import Data.Symbol (class IsSymbol)
import Data.Validated (Validated)
import Data.Variant (Variant, inj)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

data Terror error = Terror error (Array String)

type TerrorVar error = Terror (Variant error)

type TerrorNea error = Terror (NonEmptyArray error)

type TerrorNeaVar errors = Terror (NonEmptyArray (Variant errors))

type ValidatedTerror errors = Validated (Terror errors)

type ValidatedTerrorNea errors = Validated (TerrorNea errors)

type ValidatedTerrorNeaVar errors = Validated (TerrorNeaVar errors)

type NeaVar errors = NonEmptyArray (Variant errors)

type AsyncTerrorNeaVar errors = Async (TerrorNeaVar errors)

instance (Semigroup error) => Semigroup (Terror error) where
    append (Terror error lines) (Terror error' lines') =
        Terror (error <> error') (lines <> lines')

instance Functor Terror where
    map mapper (Terror error lines) = Terror (mapper error) lines

singleton :: forall error. error -> String -> Terror error
singleton error line = Terror error [line]

singletonNea :: forall error. error -> String -> Terror (NonEmptyArray error)
singletonNea error line = Terror (Nea.singleton error) [line]

toNea :: forall error. Terror error -> Terror (NonEmptyArray error)
toNea (Terror error lines) = Terror (Nea.singleton error) lines

label :: forall label error errors' errors.
    Cons label error errors' errors => IsSymbol label =>
    Proxy label -> Terror error -> TerrorVar errors
label label' terror = map (inj label') terror

labelNea :: forall error label errors' errors.
    Cons label error errors' errors => IsSymbol label =>
    Proxy label -> Terror error -> TerrorNeaVar errors
labelNea label' terror = map (inj label' >>> Nea.singleton) terror

collect :: forall error.
    NonEmptyArray (Terror error) -> Terror (NonEmptyArray error)
collect = map toNea >>> fold1

elaborate :: forall error. String -> Terror error -> Terror error
elaborate line (Terror error lines) = Terror error (Array.cons line lines)
