module TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile where

import Prelude

import Async (Async)
import Async.Validated as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label, relabel)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.Infrastructure.ValidateAbout (validateAbout)

type Profile =
    { platform :: Platform
    , fieldValues :: Array ValidateFieldValues.FieldValue
    , newOrReturning :: Boolean
    , about :: Text
    }

type ProfileError = Variant
    ( url :: { message :: Array String, key :: String }
    , about :: Array String
    )

type ProfileErrors = NonEmptyList ProfileError

validateProfile
    :: forall errors
    .  LoadFields.Game
    -> ReadProfile.Profile
    -> Async (Variant (profile :: ProfileErrors | errors)) Profile
validateProfile
    { platforms, fields }
    { platform, fieldValues, newOrReturning, about } =
    { platform, fieldValues: _, newOrReturning, about: _ }
    <$> validateFieldValues fields fieldValues
    <*> validateAbout about
    # Async.fromValidated
    # label (SProxy :: SProxy "profile")

validateProfileV
    :: forall errors
    .  LoadFields.Game
    -> ReadProfile.Profile
    -> AsyncV (NonEmptyList (Variant (playerProfile :: ProfileErrors | errors))) Profile
validateProfileV game =
    validateProfile game
    >>> relabel (SProxy :: SProxy "profile") (SProxy :: SProxy "playerProfile")
    >>> lmap NonEmptyList.singleton
    >>> AsyncV.fromAsync
