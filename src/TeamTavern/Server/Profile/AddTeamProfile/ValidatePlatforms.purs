module TeamTavern.Server.Profile.AddTeamProfile.ValidatePlatforms where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, elem)
import Data.Array.NonEmpty as NonEmptyArray
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Validated as Validated
import Data.Validated.Label (ValidatedVariants)
import Data.Variant (inj)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import Type.Proxy (Proxy(..))

validatePlatforms
    :: forall errors
    .  Platforms
    -> Array Platform
    -> ValidatedVariants (platforms :: Array String | errors) (NonEmptyArray Platform)
validatePlatforms allPlatforms selectedPlatforms = let
    nonEmptyAllPlatforms = NonEmptyArray.cons' allPlatforms.head allPlatforms.tail
    filteredSelectedPlatforms = selectedPlatforms # Array.filter (flip elem nonEmptyAllPlatforms)
    in
    case NonEmptyArray.fromArray filteredSelectedPlatforms of
    Nothing ->  Validated.invalid $ NonEmptyList.singleton $ inj (Proxy :: _ "platforms")
        [ "Error validating platforms, team profile provided no valid platforms."
        , "Game platforms: " <> show allPlatforms
        , "Profile platforms: " <> show selectedPlatforms
        ]
    Just nonEmptySelectedPlatforms -> Validated.valid nonEmptySelectedPlatforms
