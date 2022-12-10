module TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Symbol (class IsSymbol)
import Jarilo (badRequest_)
import Prim.Row (class Cons)
import TeamTavern.Routes.Profile.AddTeamProfile as AddTeamProfile
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileError)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNea, TerrorNeaVar)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.AddTeamProfile.ValidatePlatforms (validatePlatforms)
import TeamTavern.Server.Profile.Infrastructure.ValidateAbout (validateAbout)
import TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions (validateAmbitions)
import Type.Proxy (Proxy)

type Profile =
    { size :: Size
    , platforms :: NonEmptyArray Platform
    , fieldValues :: Array ValidateFieldValues.FieldValue
    , newOrReturning :: Boolean
    , about :: Text
    , ambitions :: Text
    }

type ProfileErrors = NonEmptyArray TeamProfileError

validateProfile'
    :: LoadFields.Game
    -> AddTeamProfile.RequestContentProfile
    -> ValidatedTerrorNea TeamProfileError Profile
validateProfile' game profile @ { size, newOrReturning } =
    { size, platforms: _, fieldValues: _, newOrReturning, about: _, ambitions: _ }
    <$> validatePlatforms game.platforms profile.platforms
    <*> (pure $ validateFieldValues game.fields profile.fieldValues)
    <*> validateAbout profile.about
    <*> validateAmbitions profile.ambitions

validateProfile
    :: ∀ errors
    .  LoadFields.Game
    -> AddTeamProfile.RequestContentProfile
    -> Async (BadRequestTerror ProfileErrors errors) Profile
validateProfile game profile =
    validateProfile' game profile
    # AsyncVal.fromValidated
    # lmap (map badRequest_)

validateProfileV
    :: ∀ errors' errors label
    .  Cons label ProfileErrors errors' errors
    => IsSymbol label
    => LoadFields.Game
    -> AddTeamProfile.RequestContentProfile
    -> Proxy label
    -> AsyncV (TerrorNeaVar errors) Profile
validateProfileV game profile label =
    validateProfile' game profile
    # AsyncV.fromValidated
    # AsyncV.lmap (Terror.labelNea label)
