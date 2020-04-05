module TeamTavern.Server.Player.UpdateDetails.ValidateCountry
    (Country, validateCountry, validateOptionalCountry) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Infrastructure.Countries (allCountries)

newtype Country = Country String

validateCountry :: String -> Maybe Country
validateCountry country =
    if allCountries # Array.any (_ == country)
    then Just $ Country country
    else Nothing

validateOptionalCountry :: Maybe String -> Maybe Country
validateOptionalCountry country = country >>= validateCountry
