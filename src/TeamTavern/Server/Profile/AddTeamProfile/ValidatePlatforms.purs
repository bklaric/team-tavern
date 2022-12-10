module TeamTavern.Server.Profile.AddTeamProfile.ValidatePlatforms where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, elem)
import Data.Array.NonEmpty as Nea
import Data.Maybe (Maybe(..))
import Data.Validated as Validated
import Data.Variant (inj)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import Type.Proxy (Proxy(..))

validatePlatforms
    :: ∀ errors
    .  Platforms
    -> Array Platform
    -> ValidatedTerrorNeaVar (platforms :: {} | errors) (NonEmptyArray Platform)
validatePlatforms allPlatforms selectedPlatforms = let
    nonEmptyAllPlatforms = Nea.cons' allPlatforms.head allPlatforms.tail
    filteredSelectedPlatforms = selectedPlatforms # Array.filter (flip elem nonEmptyAllPlatforms)
    in
    case Nea.fromArray filteredSelectedPlatforms of
    Nothing ->  Validated.invalid $ Terror
        (Nea.singleton $ inj (Proxy :: _ "platforms") {})
        [ "Error validating platforms, team profile provided no valid platforms."
        , "Game platforms: " <> show allPlatforms
        , "Profile platforms: " <> show selectedPlatforms
        ]
    Just nonEmptySelectedPlatforms -> Validated.valid nonEmptySelectedPlatforms
