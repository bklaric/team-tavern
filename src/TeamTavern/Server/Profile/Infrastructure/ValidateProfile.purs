module TeamTavern.Server.Profile.Infrastructure.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label as Label
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Profile.Infrastructure.LoadFields as LoadFields
import TeamTavern.Server.Profile.Infrastructure.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary as ValidateSummary
import TeamTavern.Server.Profile.Infrastructure.ValidateFieldValues (ValidateFieldValuesError)
import TeamTavern.Server.Profile.Infrastructure.ValidateFieldValues as ValidateFieldValues

data Profile =
    Profile ValidateSummary.Summary (List ValidateFieldValues.FieldValue)

type ProfileError = Variant
    ( summary :: NonEmptyList NonEmptyTextError
    , fieldValues :: NonEmptyList ValidateFieldValuesError
    )

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
validateProfile fields profile @ { summary, fieldValues } =
    Profile
    <$> (ValidateSummary.validate summary
        # Validated.label (SProxy :: SProxy "summary"))
    <*> (ValidateFieldValues.validateFieldValues fields fieldValues
        # Validated.label (SProxy :: SProxy "fieldValues"))
    # Async.fromValidated
    # Label.labelMap (SProxy :: SProxy "invalidProfile") { profile, errors: _ }
