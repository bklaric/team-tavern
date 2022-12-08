module TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as AsyncVal
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Symbol (class IsSymbol)
import Jarilo (badRequest_)
import Prim.Row (class Cons)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileError)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNea, TerrorNeaVar)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.Infrastructure.ValidateAbout (validateAbout)
import TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions (validateAmbitions)
import Type.Proxy (Proxy)

type Profile =
    { platform :: Platform
    , fieldValues :: Array ValidateFieldValues.FieldValue
    , newOrReturning :: Boolean
    , about :: Text
    , ambitions :: Text
    }

type ProfileErrors = NonEmptyArray PlayerProfileError

validateProfile'
    :: LoadFields.Game
    -> AddPlayerProfile.RequestContentProfile
    -> ValidatedTerrorNea PlayerProfileError Profile
validateProfile'
    { fields }
    { platform, fieldValues, newOrReturning, about, ambitions } =
    { platform, fieldValues: _, newOrReturning, about: _, ambitions: _ }
    <$> validateFieldValues fields fieldValues
    <*> validateAbout about
    <*> validateAmbitions ambitions

validateProfile
    :: forall errors
    .  LoadFields.Game
    -> AddPlayerProfile.RequestContentProfile
    -> Async (BadRequestTerror ProfileErrors errors) Profile
validateProfile game profile =
    validateProfile' game profile
    # AsyncVal.fromValidated
    # lmap (map badRequest_)

validateProfileV
    :: forall errors' errors label
    .  Cons label ProfileErrors errors' errors
    => IsSymbol label
    => LoadFields.Game
    -> AddPlayerProfile.RequestContentProfile
    -> Proxy label
    -> AsyncV (TerrorNeaVar errors) Profile
validateProfileV game profile label =
    validateProfile' game profile
    # AsyncV.fromValidated
    # AsyncV.lmap (Terror.labelNea label)
