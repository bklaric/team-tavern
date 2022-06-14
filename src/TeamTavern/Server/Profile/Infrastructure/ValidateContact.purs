module TeamTavern.Server.Profile.Infrastructure.ValidateContact where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Symbol (class IsSymbol)
import Data.Validated as Validated
import Data.Validated.Label (ValidatedVariants)
import Data.Variant (inj)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)

validateContact
    :: forall wrapper errors errors' error label
    .  Cons label error errors' errors
    => IsSymbol label
    => Maybe String
    -> (String -> Boolean)
    -> (String -> wrapper)
    -> Proxy label
    -> (String -> error)
    -> ValidatedVariants errors (Maybe wrapper)
validateContact Nothing _ _ _ _ =
    Validated.valid Nothing
validateContact (Just contact) isValid wrapper _ _ | contact' <- trim contact, isValid contact' =
    Validated.valid $ Just $ wrapper contact'
validateContact (Just contact) _ _ sproxy error =
    Validated.invalid $ singleton $ inj sproxy $ error contact
