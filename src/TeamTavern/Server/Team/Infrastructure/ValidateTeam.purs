module TeamTavern.Server.Team.Infrastructure.ValidateTeam where

import Prelude

import Async (Async)
import Async.Validated as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Data.Validated.Label (ValidatedVariants)
import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Organization (OrganizationNW(..))
import TeamTavern.Server.Player.UpdatePlayer.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimezone (Timezone, validateTimezone)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (AgeSpan, validateAgeSpan)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateRegions (Region, validateRegions)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Team.Infrastructure.ValidateName (Name, validateName)
import TeamTavern.Server.Team.Infrastructure.ValidateWebsite (validateWebsite)

type TeamModel =
    { organization :: OrganizationNW
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
    }

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

validateOrganization
    :: forall errors
    .  OrganizationNW
    -> ValidatedVariants (name :: Array String, website :: Array String | errors) Organization
validateOrganization InformalNW = pure Informal
validateOrganization (OrganizedNW { name, website }) =
    Organized <$> ({ name: _, website: _ } <$> validateName name <*> validateWebsite website)

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

type TeamError = Variant
    ( name :: Array String
    , website :: Array String
    )

type TeamErrors = NonEmptyList TeamError

validateTeam :: forall errors. TeamModel -> Async (Variant (team :: TeamErrors | errors)) Team
validateTeam (team :: TeamModel) = let
    organization = validateOrganization team.organization
    ageSpan = validateAgeSpan team.ageFrom team.ageTo
    locations = validateRegions team.locations
    languages = validateLanguages team.languages
    microphone = team.microphone
    timezone = validateTimezone team.timezone
    onlineWeekday = timezone >>= (const $ validateTimespan team.weekdayFrom team.weekdayTo)
    onlineWeekend = timezone >>= (const $ validateTimespan team.weekendFrom team.weekendTo)
    in
    { organization: _
    , ageSpan, locations
    , languages, microphone
    , timezone, onlineWeekday, onlineWeekend
    }
    <$> organization
    # Async.fromValidated # label (Proxy :: _ "team")

validateTeamV :: forall errors.
    TeamModel -> AsyncV (NonEmptyList (Variant (team :: TeamErrors | errors))) Team
validateTeamV = validateTeam >>> lmap NonEmptyList.singleton >>> AsyncV.fromAsync
