module TeamTavern.Server.Profile.Infrastructure.ValidateContact where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Symbol (class IsSymbol)
import Data.Validated as Validated
import Data.Variant (inj)
import Prim.Row (class Cons)
import TeamTavern.Server.Infrastructure.Error (TavernErrorMany(..), ValidatedTavern)
import Type.Proxy (Proxy)

validateContact
    :: forall wrapper errors errors' label
    .  Cons label {} errors' errors
    => IsSymbol label
    => Maybe String
    -> (String -> Boolean)
    -> (String -> wrapper)
    -> Proxy label
    -> (String -> String)
    -> ValidatedTavern errors (Maybe wrapper)
validateContact Nothing _ _ _ _ =
    Validated.valid Nothing
validateContact (Just contact) isValid wrapper _ _ | contact' <- trim contact, isValid contact' =
    Validated.valid $ Just $ wrapper contact'
validateContact (Just contact) _ _ sproxy error =
    Validated.invalid $ TavernErrorMany (singleton $ inj sproxy {}) [error contact]
