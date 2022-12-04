module TeamTavern.Server.Player.UpdateContacts.ValidateContacts where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array as Array
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
import Jarilo (badRequest_)
import TeamTavern.Routes.Player.UpdatePlayerContacts (UpdatePlayerContacts)
import TeamTavern.Routes.Player.UpdatePlayerContacts as UpdatePlayerContacts
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError, PlayerContactsErrorRow)
import TeamTavern.Server.Infrastructure.Error (TavernError(..), TavernErrorMany(..), ValidatedNelTavern, ValidatedTavern, BadRequestError, mapError, mapErrorMany', mergeAll)
import TeamTavern.Server.Infrastructure.Error as TavernError
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, validateBattleTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateDiscordTag (DiscordTag, validateDiscordTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateEaId (EaId, validateEaId)
import TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, validateFriendCode)
import TeamTavern.Server.Profile.Infrastructure.ValidateGamerTag (GamerTag, validateGamerTag)
import TeamTavern.Server.Profile.Infrastructure.ValidatePsnId (PsnId, validatePsnId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, validateSteamId)
import TeamTavern.Server.Profile.Infrastructure.ValidateUbisoftUsername (UbisoftUsername, validateUbisoftUsername)
import Type.Proxy (Proxy(..))

type Contacts =
    { discordTag :: Maybe DiscordTag
    , steamId :: Maybe SteamId
    , riotId :: Maybe RiotId
    , battleTag :: Maybe BattleTag
    , eaId :: Maybe EaId
    , ubisoftUsername :: Maybe UbisoftUsername
    , psnId :: Maybe PsnId
    , gamerTag :: Maybe GamerTag
    , friendCode :: Maybe FriendCode
    }

type ContactsErrors = Array PlayerContactsError

checkRequiredPlatforms ::
    Array Platform -> PlayerContacts -> ValidatedTavern _ Unit
checkRequiredPlatforms requiredPlatforms contacts = let
    checkPlatform errors platform =
        case platform, contacts of
        Steam,       { steamId:         Nothing } -> TavernError (inj (Proxy :: _ "steamId")         {}) ["SteamId is required"]                  : errors
        Riot,        { riotId:          Nothing } -> TavernError (inj (Proxy :: _ "riotId")          {}) ["RiotId is required"]                   : errors
        BattleNet,   { battleTag:       Nothing } -> TavernError (inj (Proxy :: _ "battleTag")       {}) ["BattleTag is required"]                : errors
        Origin,      { eaId:            Nothing } -> TavernError (inj (Proxy :: _ "eaId")            {}) ["EA ID is required"]                    : errors
        Ubisoft,     { ubisoftUsername: Nothing } -> TavernError (inj (Proxy :: _ "ubisoftUsername") {}) ["Ubisoft Connect username is required"] : errors
        PlayStation, { psnId:           Nothing } -> TavernError (inj (Proxy :: _ "psnId")           {}) ["PsnId is required"]                    : errors
        Xbox,        { gamerTag:        Nothing } -> TavernError (inj (Proxy :: _ "gamerTag")        {}) ["GamerTag is required"]                 : errors
        Switch,      { friendCode:      Nothing } -> TavernError (inj (Proxy :: _ "friendCode")      {}) ["FriendCode is required"]               : errors
        _, _ -> errors
    in
    case requiredPlatforms # foldl checkPlatform Nil of
    Nil -> Validated.valid unit
    Cons head tail -> Validated.invalid $ TavernError.collect $ NonEmptyList $ head :| tail

validateContacts'
    :: Array Platform
    -> PlayerContacts
    -> Validated (TavernErrorMany PlayerContactsErrorRow) Contacts
validateContacts' requiredPlatforms contacts @ { discordTag, steamId, riotId, battleTag, eaId, ubisoftUsername, psnId, gamerTag, friendCode } =
    { discordTag: _, steamId: _, riotId: _, battleTag: _, eaId: _, ubisoftUsername: _, psnId: _, gamerTag: _, friendCode: _ }
    <$ checkRequiredPlatforms requiredPlatforms contacts
    <*> validateDiscordTag discordTag
    <*> validateSteamId steamId
    <*> validateRiotId riotId
    <*> validateBattleTag battleTag
    <*> validateEaId eaId
    <*> validateUbisoftUsername ubisoftUsername
    <*> validatePsnId psnId
    <*> validateGamerTag gamerTag
    <*> validateFriendCode friendCode

validateContacts :: forall errors.
    Array Platform -> PlayerContacts -> Async (BadRequestError ContactsErrors errors) Contacts
validateContacts requiredPlatforms contacts =
    validateContacts' requiredPlatforms contacts
    # AsyncVal.fromValidated
    # lmap (mapErrorMany' (Array.fromFoldable >>> badRequest_))

-- validateContactsV :: forall errors.
--     Array Platform -> PlayerContacts -> AsyncV (Variants (playerContacts :: ContactsErrors | errors)) Contacts
validateContactsV :: forall errors.
    Array Platform -> PlayerContacts-> AsyncV _ Contacts
validateContactsV requiredPlatforms = validateContacts' requiredPlatforms >>> AsyncV.fromValidated
