module TeamTavern.Server.Profile.AddPlayerProfile.ValidateExternalId (ExternalId, validateExternalId) where

import Prelude

import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Symbol (SProxy(..))
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Validated.Label as ValidatedL
import Data.Variant (Variant, inj)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (validateUrl)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl as ValidateUrl

newtype ExternalId = ExternalId String

validateExternalId
    :: forall errors
    .  Int
    -> String
    -> Validated (NonEmptyList (Variant (externalId :: Array String | errors))) ExternalId
validateExternalId externalIdIlk externalId =
    case externalIdIlk of
    1 -> validateUrl "steamcommunity.com" externalId
        <#> (ValidateUrl.toString >>> ExternalId)
        # Validated.fromEither
        # ValidatedL.labelMap (SProxy :: SProxy "externalId")
            \errors -> [ "Invalid Steam profile URL: " <> show errors ]
    2 -> pure $ ExternalId externalId
    _ -> Validated.invalid $ NEL.singleton $ inj (SProxy :: SProxy "externalId") []
