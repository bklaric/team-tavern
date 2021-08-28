module TeamTavern.Server.Player.UpdateContacts.ValidateContacts where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List.NonEmpty as Nel
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Validated.Label (Variants)
import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Player as Routes
import TeamTavern.Server.Player.UpdatePlayer.ValidateDiscordTag (DiscordTag, validateDiscordTag')
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, validateBattleTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, validateFriendCode)
import TeamTavern.Server.Profile.Infrastructure.ValidateGamerTag (GamerTag, validateGamerTag)
import TeamTavern.Server.Profile.Infrastructure.ValidatePsnId (PsnId, validatePsnId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, validateSteamId)

type Contacts =
    { discordTag :: Maybe DiscordTag
    , steamId :: Maybe SteamId
    , riotId :: Maybe RiotId
    , battleTag :: Maybe BattleTag
    , psnId :: Maybe PsnId
    , gamerTag :: Maybe GamerTag
    , friendCode :: Maybe FriendCode
    }

type ContactsErrors = NonEmptyList Routes.ContactsError

validateContacts :: forall errors.
    Routes.Contacts -> Async (Variant (playerContacts :: ContactsErrors | errors)) Contacts
validateContacts { discordTag, steamId, riotId, battleTag, psnId, gamerTag, friendCode } =
    { discordTag: _, steamId: _, riotId: _, battleTag: _, psnId: _, gamerTag: _, friendCode: _ }
    <$> validateDiscordTag' discordTag
    <*> validateSteamId steamId
    <*> validateRiotId riotId
    <*> validateBattleTag battleTag
    <*> validatePsnId psnId
    <*> validateGamerTag gamerTag
    <*> validateFriendCode friendCode
    # AsyncVal.fromValidated
    # label (SProxy :: _ "playerContacts")

validateContactsV :: forall errors.
    Routes.Contacts -> AsyncV (Variants (playerContacts :: ContactsErrors | errors)) Contacts
validateContactsV = validateContacts >>> lmap Nel.singleton >>> AsyncV.fromAsync
