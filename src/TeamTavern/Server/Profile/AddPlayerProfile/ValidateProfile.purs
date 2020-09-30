module TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label as Label
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import TeamTavern.Server.Domain.Text (TextError)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (ValidateFieldValuesError)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary as ValidateSummary

data Profile =
    Profile ValidateSummary.Summary (List ValidateFieldValues.FieldValue) Boolean

type ProfileError = Variant
    ( summary :: NonEmptyList TextError
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
validateProfile fields profile @ { summary, fieldValues, newOrReturning } =
    Profile
    <$> (ValidateSummary.validate summary
        # Validated.label (SProxy :: SProxy "summary"))
    <*> (ValidateFieldValues.validateFieldValues fields fieldValues
        # Validated.label (SProxy :: SProxy "fieldValues"))
    <*> pure newOrReturning
    # Async.fromValidated
    # Label.labelMap (SProxy :: SProxy "invalidProfile") { profile, errors: _ }
