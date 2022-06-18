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
import Data.Variant (Variant)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Player.Domain.Nickname (Nickname, validateNickname)
import TeamTavern.Server.Player.Domain.Password (Password, validatePassword)
import Type.Proxy (Proxy(..))

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
    RegisterPlayer.RequestContent -> Async (Variant (registration :: RegistrationErrors | errors)) Registration
validateRegistration { nickname, password } =
    { nickname: _, password: _ }
    <$> validateNickname nickname
    <*> validatePassword password
    # Async.fromValidated
    # label (Proxy :: _ "registration")

validateRegistrationV
    :: forall errors
    .  RegisterPlayer.RequestContent
    -> AsyncV (NonEmptyList (Variant (registration :: RegistrationErrors | errors))) Registration
validateRegistrationV = validateRegistration >>> lmap NonEmptyList.singleton >>> AsyncV.fromAsync
