module TeamTavern.Server.Profile.AddGameTeam.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label as Label
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Player.UpdateDetails.ValidateLangugase (Language, validateLanguages)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (Timespan, validateTimespan)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimezone (Timezone, validateOptionalTimezone)
import TeamTavern.Server.Profile.AddGameTeam.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddGameTeam.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddGameTeam.ValidateAgeSpan (AgeSpan, validateAgeSpan)
import TeamTavern.Server.Profile.AddGameTeam.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddGameTeam.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.AddGameTeam.ValidateRegions (Region, validateRegions)
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary as ValidateSummary

type Profile =
    { summary :: ValidateSummary.Summary
    , ageSpan :: AgeSpan
    , languages :: Array Language
    , regions :: Array Region
    , timezone :: Maybe Timezone
    , onlineWeekday :: Maybe Timespan
    , onlineWeekend :: Maybe Timespan
    , hasMicrophone :: Boolean
    , fieldValues :: Array ValidateFieldValues.FieldValue
    }

type ProfileError = Variant (summary :: NonEmptyList NonEmptyTextError)

type ValidateProfileError errors = Variant
    ( invalidProfile ::
        { profile :: ReadProfile.Profile
        , errors :: NonEmptyList ProfileError
        }
    | errors )

validateProfile
    :: forall errors
    .  Array LoadFields.Field
    -> ReadProfile.Profile
    -> Async (ValidateProfileError errors) Profile
validateProfile fields profile @ { summary } = let
    ageSpan = validateAgeSpan profile.ageFrom profile.ageTo
    languages = validateLanguages profile.languages
    regions = validateRegions profile.regions
    timezone = validateOptionalTimezone profile.timezone
    onlineWeekday = timezone >>=
        (const $ validateTimespan profile.weekdayFrom profile.weekdayTo)
    onlineWeekend = timezone >>=
        (const $ validateTimespan profile.weekendFrom profile.weekendTo)
    hasMicrophone = profile.hasMicrophone
    fieldValues = validateFieldValues fields profile.fieldValues
    in
    { summary: _, ageSpan, languages, regions, timezone
    , onlineWeekday, onlineWeekend, hasMicrophone, fieldValues
    }
    <$> (ValidateSummary.validate summary
        # Validated.label (SProxy :: SProxy "summary"))
    # Async.fromValidated
    # Label.labelMap (SProxy :: SProxy "invalidProfile") { profile, errors: _ }
