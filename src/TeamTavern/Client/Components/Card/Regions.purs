module TeamTavern.Client.Components.Card.Regions (regionCount, regionShortName, regionsText) where

import Prelude

import Data.Array (length)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-- What a card calls each of the twelve regions. No code fits all twelve: EU and
-- NA are read everywhere, but SA is South America and South Asia at once, so a
-- name cut at the compass point is what stays legible.
shortNames :: Object.Object String
shortNames = Object.fromFoldable
    [ Tuple "Europe" "Europe"
    , Tuple "Middle East" "Middle East"
    , Tuple "North Africa" "N. Africa"
    , Tuple "Sub-Saharan Africa" "Sub-Saharan Africa"
    , Tuple "North America" "N. America"
    , Tuple "Central America" "C. America"
    , Tuple "South America" "S. America"
    , Tuple "Central Asia" "C. Asia"
    , Tuple "South Asia" "S. Asia"
    , Tuple "East Asia" "E. Asia"
    , Tuple "Southeast Asia" "SE Asia"
    , Tuple "Oceania" "Oceania"
    ]

regionShortName :: String -> String
regionShortName region = Object.lookup region shortNames # fromMaybe region

regionCount :: Int
regionCount = Object.size shortNames

-- | A post's regions as its fact line reads them. All twelve read Anywhere.
regionsText :: Array String -> String
regionsText regions
    | length regions == regionCount = "Anywhere"
    | otherwise = regions <#> regionShortName # joinWith ", "
