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
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Validated.Label (Variants)
import Data.Variant (Variant, inj)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsError, PlayerContacts)
import TeamTavern.Server.Player.UpdatePlayer.ValidateDiscordTag (DiscordTag, validateDiscordTag')
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, validateBattleTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateEaId (EaId, validateEaId)
import TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, validateFriendCode)
import TeamTavern.Server.Profile.Infrastructure.ValidateGamerTag (GamerTag, validateGamerTag)
import TeamTavern.Server.Profile.Infrastructure.ValidatePsnId (PsnId, validatePsnId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, validateSteamId)
import Type.Proxy (Proxy(..))

type Contacts =
    { discordTag :: Maybe DiscordTag
    , steamId :: Maybe SteamId
    , riotId :: Maybe RiotId
    , battleTag :: Maybe BattleTag
    , eaId :: Maybe EaId
    , psnId :: Maybe PsnId
    , gamerTag :: Maybe GamerTag
    , friendCode :: Maybe FriendCode
    }

type ContactsErrors = NonEmptyList PlayerContactsError

checkRequiredPlatforms :: Array Platform -> PlayerContacts -> Validated ContactsErrors Unit
checkRequiredPlatforms requiredPlatforms contacts = let
    checkPlatform errors platform =
            case platform, contacts of
            Steam, { steamId: Nothing } -> inj (Proxy :: _ "steamId") "SteamId is required" : errors
            Riot, { riotId: Nothing } -> inj (Proxy :: _ "riotId") "RiotId is required" : errors
            BattleNet, { battleTag: Nothing } -> inj (Proxy :: _ "battleTag") "BattleTag is required" : errors
            Origin, { eaId: Nothing } -> inj (Proxy :: _ "eaId") "EA ID is required" : errors
            PlayStation, { psnId: Nothing } -> inj (Proxy :: _ "psnId") "PsnId is required" : errors
            Xbox, { gamerTag: Nothing } -> inj (Proxy :: _ "gamerTag") "GamerTag is required" : errors
            Switch, { friendCode: Nothing } -> inj (Proxy :: _ "friendCode") "FriendCode is required" : errors
            _, _ -> errors
    in
    case requiredPlatforms # foldl checkPlatform Nil of
    Nil -> Validated.valid unit
    Cons head tail -> Validated.invalid $ NonEmptyList $ head :| tail

validateContacts :: forall errors.
    Array Platform -> PlayerContacts -> Async (Variant (playerContacts :: ContactsErrors | errors)) Contacts
validateContacts requiredPlatforms contacts @ { discordTag, steamId, riotId, battleTag, eaId, psnId, gamerTag, friendCode } =
    { discordTag: _, steamId: _, riotId: _, battleTag: _, eaId: _, psnId: _, gamerTag: _, friendCode: _ }
    <$ checkRequiredPlatforms requiredPlatforms contacts
    <*> validateDiscordTag' discordTag
    <*> validateSteamId steamId
    <*> validateRiotId riotId
    <*> validateBattleTag battleTag
    <*> validateEaId eaId
    <*> validatePsnId psnId
    <*> validateGamerTag gamerTag
    <*> validateFriendCode friendCode
    # AsyncVal.fromValidated
    # label (Proxy :: _ "playerContacts")

validateContactsV :: forall errors.
    Array Platform -> PlayerContacts -> AsyncV (Variants (playerContacts :: ContactsErrors | errors)) Contacts
validateContactsV requiredPlatforms = validateContacts requiredPlatforms >>> lmap Nel.singleton >>> AsyncV.fromAsync
