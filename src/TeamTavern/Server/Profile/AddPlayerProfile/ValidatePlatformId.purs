module TeamTavern.Server.Profile.AddPlayerProfile.ValidatePlatformId (PlatformId, toString, validatePlatformId) where

import Prelude

import Data.Array as Array
import Data.List.NonEmpty (notElem)
import Data.List.NonEmpty as NEL
import Data.Symbol (SProxy(..))
import Data.Validated as Validated
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as ValidatedL
import Data.Variant (inj)
import TeamTavern.Routes.Shared.Platform (Platform(..), Platforms)
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, validateBattleTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag as BattleTag
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId as RiotId
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamUrl (validateSteamUrl)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl as Url

data PlatformId = SteamUrl Url | RiotId RiotId | BattleTag BattleTag

toString :: PlatformId -> String
toString (SteamUrl url) = Url.toString url
toString (RiotId riotId) = RiotId.toString riotId
toString (BattleTag battleTag) = BattleTag.toString battleTag

validatePlatformId
    :: forall errors
    .  Platforms
    -> Platform
    -> String
    -> VariantValidated (platformId :: Array String | errors) PlatformId
validatePlatformId platforms platform platformId =
    case platform of
    _ | notElem platform $ Array.cons platforms.head platforms.tail ->
        Validated.invalid $ NEL.singleton $ inj (SProxy :: SProxy "platformId")
        [ "Error validating platform id, profile platform doesn't match any game platforms."
        , "Profile platform: " <> show platform
        , "Game platforms: " <> show platforms
        ]
    Steam -> validateSteamUrl platformId
        <#> SteamUrl
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "platformId")
    Riot -> validateRiotId platformId
        <#> RiotId
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "platformId")
    BattleNet -> validateBattleTag platformId
        <#> BattleTag
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "platformId")
    ilk -> Validated.invalid $ NEL.singleton $ inj (SProxy :: SProxy "platformId")
        [ "Error validating platform id, platform is unknown: " <> show ilk ]
