module TeamTavern.Server.Profile.Infrastructure.ValidateSteamUrl where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.List.NonEmpty as Nel
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Validated (invalid, valid)
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as LabelV
import Data.Variant (inj)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, validateUrl, validateUrlV)

validateSteamUrl :: String -> Either (Array String) Url
validateSteamUrl steamUrl =
    validateUrl "steamcommunity.com" steamUrl
    # lmap \errors -> [ "Invalid Steam profile URL: " <> show errors ]

validateSteamUrl' :: forall errors.
   Maybe String -> Boolean -> VariantValidated (steamUrl :: Array String | errors) (Maybe Url)
validateSteamUrl' Nothing false = valid Nothing
validateSteamUrl' Nothing true = invalid $ Nel.singleton $ inj (SProxy :: SProxy "steamUrl")
    [ "No Steam URL provided, but it is required." ]
validateSteamUrl' (Just steamUrl) _ =
    validateUrlV "steamcommunity.com" steamUrl
    <#> Just
    # LabelV.labelMap (SProxy :: SProxy "steamUrl") \errors ->
        [ "Invalid Steam URL: " <> steamUrl, "Failed with following errors: " <> show errors ]
