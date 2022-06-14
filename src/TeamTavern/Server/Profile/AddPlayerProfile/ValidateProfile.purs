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
import Type.Proxy (Proxy(..))
import Data.Variant (Variant)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (validateFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.Infrastructure.ValidateAbout (validateAbout)
import TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions (validateAmbitions)

type Profile =
    { platform :: Platform
    , fieldValues :: Array ValidateFieldValues.FieldValue
    , newOrReturning :: Boolean
    , about :: Text
    , ambitions :: Text
    }

type ProfileError = Variant
    ( url :: { message :: Array String, key :: String }
    , about :: Array String
    , ambitions :: Array String
    )

type ProfileErrors = NonEmptyList ProfileError

validateProfile
    :: forall errors
    .  LoadFields.Game
    -> ReadProfile.Profile
    -> Async (Variant (profile :: ProfileErrors | errors)) Profile
validateProfile
    { fields }
    { platform, fieldValues, newOrReturning, about, ambitions } =
    { platform, fieldValues: _, newOrReturning, about: _, ambitions: _ }
    <$> validateFieldValues fields fieldValues
    <*> validateAbout about
    <*> validateAmbitions ambitions
    # Async.fromValidated
    # label (Proxy :: _ "profile")

validateProfileV
    :: forall errors
    .  LoadFields.Game
    -> ReadProfile.Profile
    -> AsyncV (NonEmptyList (Variant (playerProfile :: ProfileErrors | errors))) Profile
validateProfileV game =
    validateProfile game
    >>> relabel (Proxy :: _ "profile") (Proxy :: _ "playerProfile")
    >>> lmap NonEmptyList.singleton
    >>> AsyncV.fromAsync
