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
import TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, validateFriendCode)
import TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode as FriendCode
import TeamTavern.Server.Profile.Infrastructure.ValidateGamertag (Gamertag, validateGamertag)
import TeamTavern.Server.Profile.Infrastructure.ValidateGamertag as Gamertag
import TeamTavern.Server.Profile.Infrastructure.ValidatePsnId (PsnId, validatePsnId)
import TeamTavern.Server.Profile.Infrastructure.ValidatePsnId as PsnId
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId as RiotId
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, validateSteamId)
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamId as SteamId

data PlatformId
    = SteamId SteamId
    | RiotId RiotId
    | BattleTag BattleTag
    | PsnId PsnId
    | Gamertag Gamertag
    | FriendCode FriendCode

toString :: PlatformId -> String
toString (SteamId steamId) = SteamId.toString steamId
toString (RiotId riotId) = RiotId.toString riotId
toString (BattleTag battleTag) = BattleTag.toString battleTag
toString (PsnId psnId) = PsnId.toString psnId
toString (Gamertag gamertag) = Gamertag.toString gamertag
toString (FriendCode friendCode) = FriendCode.toString friendCode

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
    Steam -> validateSteamId platformId
        <#> SteamId
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
    PlayStation -> validatePsnId platformId
        <#> PsnId
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "platformId")
    Xbox -> validateGamertag platformId
        <#> Gamertag
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "platformId")
    Switch -> validateFriendCode platformId
        <#> FriendCode
        # Validated.fromEither
        # ValidatedL.label (SProxy :: SProxy "platformId")
