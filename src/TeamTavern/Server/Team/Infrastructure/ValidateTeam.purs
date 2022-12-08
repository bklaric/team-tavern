module TeamTavern.Server.Team.Infrastructure.ValidateTeam where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Jarilo (badRequest_)
import TeamTavern.Routes.Shared.Organization (OrganizationNW(..))
import TeamTavern.Routes.Team.CreateTeam as CreateTeam
import TeamTavern.Server.Infrastructure.Error (TerrorNeaVar, ValidatedTerror)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import TeamTavern.Server.Player.UpdatePlayer.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimezone (Timezone, validateTimezone)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (AgeSpan, validateAgeSpan)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateRegions (Region, validateRegions)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Team.Infrastructure.ValidateName (Name, validateName)
import TeamTavern.Server.Team.Infrastructure.ValidateWebsite (validateWebsite)
import Type.Proxy (Proxy(..))

data Organization = Informal | Organized { name :: Name, website :: Maybe Url }

organizationName :: Organization -> Maybe Name
organizationName Informal = Nothing
organizationName (Organized { name }) = Just name

organizationWebsite :: Organization -> Maybe Url
organizationWebsite Informal = Nothing
organizationWebsite (Organized { website }) = website

toString :: Organization -> String
toString Informal = "informal"
toString (Organized _) = "organized"

validateOrganization ::
    OrganizationNW -> ValidatedTerror CreateTeam.BadContent Organization
validateOrganization InformalNW = pure Informal
validateOrganization (OrganizedNW { name, website }) =
    Organized
    <$> ({ name: _, website: _ }
    <$> validateName name
    <*> validateWebsite website)

type Team =
    { organization :: Organization
    , ageSpan :: AgeSpan
    , locations :: Array Region
    , languages :: Array Language
    , microphone :: Boolean
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    }

validateTeam' ::
    CreateTeam.RequestContent -> ValidatedTerror CreateTeam.BadContent Team
validateTeam' team = let
    organization = validateOrganization team.organization
    ageSpan = validateAgeSpan team.ageFrom team.ageTo
    locations = validateRegions team.locations
    languages = validateLanguages team.languages
    microphone = team.microphone
    timezone = validateTimezone team.timezone
    onlineWeekday =
        timezone >>= (const $ validateTimespan team.weekdayFrom team.weekdayTo)
    onlineWeekend =
        timezone >>= (const $ validateTimespan team.weekendFrom team.weekendTo)
    in
    { organization: _
    , ageSpan, locations
    , languages, microphone
    , timezone, onlineWeekday, onlineWeekend
    }
    <$> organization

validateTeam
    :: forall errors
    .  CreateTeam.RequestContent
    -> Async (BadRequestTerror CreateTeam.BadContent errors) Team
validateTeam team =
    validateTeam' team
    # AsyncVal.fromValidated
    # lmap (map badRequest_)

validateTeamV
    :: forall errors
    .  CreateTeam.RequestContent
    -> AsyncV (TerrorNeaVar (team :: CreateTeam.BadContent | errors)) Team
validateTeamV team = validateTeam'
    team
    # AsyncV.fromValidated
    # AsyncV.lmap (Terror.labelNea (Proxy :: _ "team"))
