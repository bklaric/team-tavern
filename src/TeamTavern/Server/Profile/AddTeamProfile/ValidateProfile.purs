module TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAmbitions (validateAmbitions)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues as ValidateFieldValues

type Profile =
    { fieldValues :: Array ValidateFieldValues.FieldValue
    , newOrReturning :: Boolean
    , ambitions :: Text
    }

type ProfileError = Variant (ambitions :: Array String)

type ProfileErrors = NonEmptyList ProfileError

validateProfile
    :: forall errors
    .  Array LoadFields.Field
    -> ReadProfile.Profile
    -> Async (Variant (profile :: ProfileErrors | errors)) Profile
validateProfile fields profile = let
    fieldValues = validateFieldValues fields profile.fieldValues
    newOrReturning = profile.newOrReturning
    ambitions = validateAmbitions profile.ambitions
    in
    { fieldValues, newOrReturning, ambitions: _ }
    <$> ambitions
    # Async.fromValidated # label (SProxy :: SProxy "profile")
