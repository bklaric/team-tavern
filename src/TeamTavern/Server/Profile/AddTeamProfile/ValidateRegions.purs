module TeamTavern.Server.Profile.AddTeamProfile.ValidateRegions
    (Region, validateRegions) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import TeamTavern.Server.Infrastructure.Regions as Regions

newtype Region = Region String

isValidRegion :: Array Regions.Region -> String -> Boolean
isValidRegion regions region =
    regions # Array.any \(Regions.Region region' regions') ->
        region == region' || isValidRegion regions' region

validateRegion :: String -> Maybe Region
validateRegion region | isValidRegion Regions.allRegions region =
    Just $ Region region
validateRegion region = Nothing

validateRegions :: Array String -> Array Region
validateRegions regions = regions # Array.mapMaybe validateRegion
