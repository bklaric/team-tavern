module TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label, relabel)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.AddTeamProfile.ValidatePlatforms (validatePlatforms)
import TeamTavern.Server.Profile.Infrastructure.ValidateAbout (validateAbout)

type Profile =
    { size :: Size
    , platforms :: NonEmptyArray Platform
    , fieldValues :: Array ValidateFieldValues.FieldValue
    , newOrReturning :: Boolean
    , about :: Text
    }

type ProfileError = Variant (platforms :: Array String, about :: Array String)

type ProfileErrors = NonEmptyList ProfileError

validateProfile
    :: forall errors
    .  LoadFields.Game
    -> ReadProfile.Profile
    -> Async (Variant (profile :: ProfileErrors | errors)) Profile
validateProfile game profile @ { size, newOrReturning } =
    { size, platforms: _, fieldValues: _, newOrReturning, about: _ }
    <$> validatePlatforms game.platforms profile.platforms
    <*> (pure $ validateFieldValues game.fields profile.fieldValues)
    <*> validateAbout profile.about
    # Async.fromValidated # label (SProxy :: SProxy "profile")

validateProfileV
    :: forall errors
    .  LoadFields.Game
    -> ReadProfile.Profile
    -> AsyncV (NonEmptyList (Variant (teamProfile :: ProfileErrors | errors))) Profile
validateProfileV game =
    validateProfile game
    >>> relabel (SProxy :: SProxy "profile") (SProxy :: SProxy "teamProfile")
    >>> lmap NonEmptyList.singleton
    >>> AsyncV.fromAsync
