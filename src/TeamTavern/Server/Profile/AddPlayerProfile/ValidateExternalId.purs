module TeamTavern.Server.Profile.AddPlayerProfile.ValidateExternalId (ExternalId, toString, validateExternalId) where

import Prelude

import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Symbol (SProxy(..))
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Validated.Label as ValidatedL
import Data.Variant (Variant, inj)
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, validateBattleTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag as BattleTag
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId as RiotId
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamUrl (validateSteamUrl)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl as Url

data ExternalId = SteamUrl Url | RiotId RiotId | BattleTag BattleTag

toString :: ExternalId -> String
toString (SteamUrl url) = Url.toString url
toString (RiotId riotId) = RiotId.toString riotId
toString (BattleTag battleTag) = BattleTag.toString battleTag

validateExternalId
    :: forall errors
    .  Int
    -> String
    -> Validated (NonEmptyList (Variant (externalId :: Array String | errors))) ExternalId
validateExternalId externalIdIlk externalId =
    case externalIdIlk of
    1 -> validateSteamUrl externalId
        <#> SteamUrl
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "externalId")
    2 -> validateRiotId externalId
        <#> RiotId
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "externalId")
    3 -> validateBattleTag externalId
        <#> BattleTag
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "externalId")
    ilk -> Validated.invalid $ NEL.singleton $ inj (SProxy :: SProxy "externalId")
        [ "Error validating external id, ilk is unknown: " <> show ilk ]
