module TeamTavern.Server.Player.UpdatePlayer.ValidateLocation (Location, validateLocation) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)

newtype Location = Location String

validateLocation' :: String -> Region -> Maybe Location
validateLocation' location (Region validLocation subLocations) =
    if location == validLocation
    then Just $ Location location
    else subLocations # Array.findMap (validateLocation' location)

validateLocation :: Maybe String -> Maybe Location
validateLocation location = location >>= \location' ->
    allRegions # Array.findMap (validateLocation' location')
