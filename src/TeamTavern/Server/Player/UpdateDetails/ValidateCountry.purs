module TeamTavern.Server.Player.UpdateDetails.ValidateCountry
    (Country, validateCountry, validateOptionalCountry) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)

newtype Country = Country String

validateLocation :: String -> Region -> Maybe Country
validateLocation location (Region validLocation subLocations) =
    if location == validLocation
    then Just $ Country location
    else subLocations # Array.findMap (validateLocation location)

validateCountry :: String -> Maybe Country
validateCountry country = allRegions # Array.findMap (validateLocation country)

validateOptionalCountry :: Maybe String -> Maybe Country
validateOptionalCountry country = country >>= validateCountry
