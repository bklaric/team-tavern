module TeamTavern.Server.Player.Register.ValidateRegistration where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Server.Player.Domain.Nickname (Nickname, validateNickname)
import TeamTavern.Server.Player.Domain.Password (Password, validatePassword)
import TeamTavern.Server.Player.Register.ReadDto (RegisterDto)

type Registration =
    { nickname :: Nickname
    , password :: Password
    }

type RegistrationError = Variant
    ( nickname :: Array String
    , password :: Array String
    )

type RegistrationErrors = NonEmptyList RegistrationError

validateRegistration :: forall errors.
    RegisterDto -> Async (Variant (registration :: RegistrationErrors | errors)) Registration
validateRegistration dto @ { nickname, password } =
    { nickname: _, password: _ }
    <$> validateNickname nickname
    <*> validatePassword password
    # Async.fromValidated
    # label (SProxy :: SProxy "registration")

validateRegistrationV
    :: forall errors
    .  RegisterDto
    -> AsyncV (NonEmptyList (Variant (registration :: RegistrationErrors | errors))) Registration
validateRegistrationV = validateRegistration >>> lmap NonEmptyList.singleton >>> AsyncV.fromAsync
