module TeamTavern.Server.Player.UpdateContacts.ValidateContacts where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List ((:))
import Data.List.NonEmpty (NonEmptyList(..), foldl)
import Data.List.NonEmpty as Nel
import Data.List.Types (List(..), NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Symbol (SProxy(..))
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Validated.Label (Variants)
import Data.Variant (Variant, inj)
import TeamTavern.Routes.Shared.Platform (Platform(..))
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

checkRequiredPlatforms :: Array Platform -> Routes.Contacts -> Validated ContactsErrors Unit
checkRequiredPlatforms requiredPlatforms contacts = let
    checkPlatform errors platform =
            case platform, contacts of
            Steam, { steamId: Nothing } -> inj (SProxy :: _ "steamId") "SteamId is required" : errors
            Riot, { riotId: Nothing } -> inj (SProxy :: _ "riotId") "RiotId is required" : errors
            BattleNet, { battleTag: Nothing } -> inj (SProxy :: _ "battleTag") "BattleTag is required" : errors
            PlayStation, { psnId: Nothing } -> inj (SProxy :: _ "psnId") "PsnId is required" : errors
            Xbox, { gamerTag: Nothing } -> inj (SProxy :: _ "gamerTag") "GamerTag is required" : errors
            Switch, { friendCode: Nothing } -> inj (SProxy :: _ "friendCode") "FriendCode is required" : errors
            _, _ -> errors
    in
    case requiredPlatforms # foldl checkPlatform Nil of
    Nil -> Validated.valid unit
    Cons head tail -> Validated.invalid $ NonEmptyList $ head :| tail

validateContacts :: forall errors.
    Array Platform -> Routes.Contacts -> Async (Variant (playerContacts :: ContactsErrors | errors)) Contacts
validateContacts requiredPlatforms contacts @ { discordTag, steamId, riotId, battleTag, psnId, gamerTag, friendCode } =
    { discordTag: _, steamId: _, riotId: _, battleTag: _, psnId: _, gamerTag: _, friendCode: _ }
    <$ checkRequiredPlatforms requiredPlatforms contacts
    <*> validateDiscordTag' discordTag
    <*> validateSteamId steamId
    <*> validateRiotId riotId
    <*> validateBattleTag battleTag
    <*> validatePsnId psnId
    <*> validateGamerTag gamerTag
    <*> validateFriendCode friendCode
    # AsyncVal.fromValidated
    # label (SProxy :: _ "playerContacts")

validateContactsV :: forall errors.
    Array Platform -> Routes.Contacts -> AsyncV (Variants (playerContacts :: ContactsErrors | errors)) Contacts
validateContactsV requiredPlatforms = validateContacts requiredPlatforms >>> lmap Nel.singleton >>> AsyncV.fromAsync
