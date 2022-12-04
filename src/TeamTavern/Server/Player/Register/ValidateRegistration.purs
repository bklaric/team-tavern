module TeamTavern.Server.Player.Register.ValidateRegistration where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Jarilo (badRequest_)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Infrastructure.Error (TavernError, BadRequestError, mapError, mergeAll)
import TeamTavern.Server.Player.Domain.Nickname (Nickname, validateNickname)
import TeamTavern.Server.Player.Domain.Password (Password, validatePassword)
import Type.Proxy (Proxy(..))

type Registration =
    { nickname :: Nickname
    , password :: Password
    }

type RegistrationError = Variant
    ( nickname :: {}
    , password :: {}
    )

type RegistrationErrors = Array RegistrationError

validateRegistration'
    :: forall errors
    .  RegisterPlayer.RequestContent
    -> Async (TavernError (registration :: Array RegistrationError | errors)) Registration
validateRegistration' { nickname, password } =
    { nickname: _, password: _ }
    <$> validateNickname nickname
    <*> validatePassword password
    # Async.fromValidated
    # lmap (mergeAll (Proxy :: _ "registration"))

validateRegistration
    :: forall errors
    .  RegisterPlayer.RequestContent
    -> Async (BadRequestError RegisterPlayer.BadContent errors) Registration
validateRegistration identifiers =
    validateRegistration' identifiers
    # lmap (mapError badRequest_)

validateRegistrationV
    :: forall errors
    .  RegisterPlayer.RequestContent
    -> AsyncV (NonEmptyList (TavernError (registration :: RegistrationErrors | errors))) Registration
validateRegistrationV = validateRegistration' >>> lmap NonEmptyList.singleton >>> AsyncV.fromAsync
