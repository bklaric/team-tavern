module TeamTavern.Server.Team.Infrastructure.ValidateTeam where

import Prelude

import Async (Async)
import Async.Validated as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List.NonEmpty as Nel
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe, isJust)
import Data.Symbol (SProxy(..))
import Data.Validated (invalid, valid, validated)
import Data.Variant (Variant, inj)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Infrastructure.ValidateAbout (validateAbout)
import TeamTavern.Server.Player.UpdatePlayer.ValidateDiscordTag (DiscordTag, validateDiscordTag)
import TeamTavern.Server.Player.UpdatePlayer.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimezone (Timezone, validateTimezone)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (AgeSpan, validateAgeSpan)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateRegions (Region, validateRegions)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Team.Infrastructure.ValidateDiscordServer (validateDiscordServer)
import TeamTavern.Server.Team.Infrastructure.ValidateName (Name, validateName)
import TeamTavern.Server.Team.Infrastructure.ValidateWebsite (validateWebsite)

type TeamModel =
    { name :: String
    , website :: Maybe String
    , discordTag :: Maybe String
    , discordServer :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

type Team =
    { name :: Name
    , website :: Maybe Url
    , discordTag :: Maybe DiscordTag
    , discordServer :: Maybe Url
    , ageSpan :: AgeSpan
    , locations :: Array Region
    , languages :: Array Language
    , microphone :: Boolean
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , about :: Text
    }

type TeamError = Variant
    ( name :: Array String
    , website :: Array String
    , discordTag :: Array String
    , discordServer :: Array String
    , contact :: Array String
    , about :: Array String
    )

type TeamErrors = NonEmptyList TeamError

validateTeam :: forall errors. TeamModel -> Async (Variant (team :: TeamErrors | errors)) Team
validateTeam (team :: TeamModel) = let
    name = validateName team.name
    website = validateWebsite team.website
    discordTag = validateDiscordTag team.discordTag
    discordServer = validateDiscordServer team.discordServer
    contact =
        if  (validated (const true) isJust discordTag)
            || (validated (const true) isJust discordServer)
        then valid unit
        else invalid $ Nel.singleton $ inj (SProxy :: SProxy "contact")
            [ "Neither Discord tag nor Discord server have been provided."
            , "Discord tag: " <> show discordTag
            , "Discord server: " <> show discordServer
            ]
    ageSpan = validateAgeSpan team.ageFrom team.ageTo
    locations = validateRegions team.locations
    languages = validateLanguages team.languages
    microphone = team.microphone
    timezone = validateTimezone team.timezone
    onlineWeekday = timezone >>= (const $ validateTimespan team.weekdayFrom team.weekdayTo)
    onlineWeekend = timezone >>= (const $ validateTimespan team.weekendFrom team.weekendTo)
    about = validateAbout team.about
    in
    { name: _, website: _
    , discordTag: _, discordServer: _
    , ageSpan, locations
    , languages, microphone
    , timezone, onlineWeekday, onlineWeekend
    , about: _
    }
    <$> name <*> website <*> discordTag <*> discordServer <*> about <* contact
    # Async.fromValidated # label (SProxy :: SProxy "team")

validateTeamV :: forall errors.
    TeamModel -> AsyncV (NonEmptyList (Variant (team :: TeamErrors | errors))) Team
validateTeamV = validateTeam >>> lmap NonEmptyList.singleton >>> AsyncV.fromAsync
