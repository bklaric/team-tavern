module TeamTavern.Server.Team.Infrastructure.ValidateContacts where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.List ((:))
import Data.List.NonEmpty (foldl)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Validated as Validated
import Data.Variant (inj)
import Jarilo (badRequest_)
import Prim.Row (class Cons)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNea, TerrorNeaVar)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, validateBattleTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateDiscordTag (DiscordTag, validateDiscordTag)
import TeamTavern.Server.Profile.Infrastructure.ValidateEaId (EaId, validateEaId)
import TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, validateFriendCode)
import TeamTavern.Server.Profile.Infrastructure.ValidateGamerTag (GamerTag, validateGamerTag)
import TeamTavern.Server.Profile.Infrastructure.ValidatePsnId (PsnId, validatePsnId)
import TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, validateRiotId)
import TeamTavern.Server.Profile.Infrastructure.ValidateSteamId (SteamId, validateSteamId)
import TeamTavern.Server.Profile.Infrastructure.ValidateUbisoftUsername (UbisoftUsername, validateUbisoftUsername)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Team.Infrastructure.ValidateDiscordServer (validateDiscordServer)
import Type.Proxy (Proxy(..))

type Contacts =
    { discordTag :: Maybe DiscordTag
    , discordServer :: Maybe Url
    , steamId :: Maybe SteamId
    , riotId :: Maybe RiotId
    , battleTag :: Maybe BattleTag
    , eaId :: Maybe EaId
    , ubisoftUsername :: Maybe UbisoftUsername
    , psnId :: Maybe PsnId
    , gamerTag :: Maybe GamerTag
    , friendCode :: Maybe FriendCode
    }

type ContactsErrors = NonEmptyArray TeamContactsError

checkRequiredPlatforms ::
    Array Platform -> TeamContacts -> ValidatedTerrorNea TeamContactsError Unit
checkRequiredPlatforms requiredPlatforms contacts = let
    checkPlatform errors platform =
        case platform, contacts of
        Steam,       { steamId: Nothing         } -> Terror (inj (Proxy :: _ "steamId")         {}) ["SteamId is required"]                  : errors
        Riot,        { riotId: Nothing          } -> Terror (inj (Proxy :: _ "riotId")          {}) ["RiotId is required"]                   : errors
        BattleNet,   { battleTag: Nothing       } -> Terror (inj (Proxy :: _ "battleTag")       {}) ["BattleTag is required"]                : errors
        Origin,      { eaId: Nothing            } -> Terror (inj (Proxy :: _ "eaId")            {}) ["EA ID is required"]                    : errors
        Ubisoft,     { ubisoftUsername: Nothing } -> Terror (inj (Proxy :: _ "ubisoftUsername") {}) ["Ubisoft Connect username is required"] : errors
        PlayStation, { psnId: Nothing           } -> Terror (inj (Proxy :: _ "psnId")           {}) ["PsnId is required"]                    : errors
        Xbox,        { gamerTag: Nothing        } -> Terror (inj (Proxy :: _ "gamerTag")        {}) ["GamerTag is required"]                 : errors
        Switch,      { friendCode: Nothing      } -> Terror (inj (Proxy :: _ "friendCode")      {}) ["FriendCode is required"]               : errors
        _, _ -> errors
    in
    case requiredPlatforms # foldl checkPlatform Nil of
    Nil -> Validated.valid unit
    Cons head tail -> Validated.invalid $ Terror.collect $ Nea.cons' head (Array.fromFoldable tail)

validateContacts'
    :: Array Platform
    -> TeamContacts
    -> ValidatedTerrorNea TeamContactsError Contacts
validateContacts' requiredPlatforms contacts @ { discordTag, discordServer, steamId, riotId, battleTag, eaId, ubisoftUsername, psnId, gamerTag, friendCode } =
    { discordTag: _, discordServer: _, steamId: _, riotId: _, battleTag: _, eaId: _, ubisoftUsername: _, psnId: _, gamerTag: _, friendCode: _ }
    <$ checkRequiredPlatforms requiredPlatforms contacts
    <*> validateDiscordTag discordTag
    <*> validateDiscordServer discordServer
    <*> validateSteamId steamId
    <*> validateRiotId riotId
    <*> validateBattleTag battleTag
    <*> validateEaId eaId
    <*> validateUbisoftUsername ubisoftUsername
    <*> validatePsnId psnId
    <*> validateGamerTag gamerTag
    <*> validateFriendCode friendCode

validateContacts :: forall errors.
    Array Platform -> TeamContacts -> Async (BadRequestTerror ContactsErrors errors) Contacts
validateContacts requiredPlatforms contacts =
    validateContacts' requiredPlatforms contacts
    # AsyncVal.fromValidated
    # lmap (map badRequest_)

validateContactsV :: forall errors' errors label.
    Cons label ContactsErrors errors' errors => IsSymbol label =>
    Array Platform -> TeamContacts -> Proxy label -> AsyncV (TerrorNeaVar errors) Contacts
validateContactsV requiredPlatforms contacts label =
    validateContacts' requiredPlatforms contacts
    # AsyncV.fromValidated
    # AsyncV.lmap (Terror.labelNea label)
