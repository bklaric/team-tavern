module TeamTavern.Server.Profile.Infrastructure.ValidateSteamUrl where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Validated (valid)
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as LabelV
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, validateUrl, validateUrlV)

validateSteamUrl :: String -> Either (Array String) Url
validateSteamUrl steamUrl =
    validateUrl "steamcommunity.com" steamUrl
    # lmap \errors -> [ "Invalid Steam profile URL: " <> show errors ]

validateSteamUrl' :: forall errors.
   Maybe String -> VariantValidated (steamUrl :: Array String | errors) (Maybe Url)
validateSteamUrl' Nothing = valid Nothing
validateSteamUrl' (Just steamUrl) =
    validateUrlV "steamcommunity.com" steamUrl
    <#> Just
    # LabelV.labelMap (SProxy :: SProxy "steamUrl") \errors ->
        [ "Invalid Steam profile URL: " <> steamUrl
        , "Failed with following errors: " <> show errors
        ]
