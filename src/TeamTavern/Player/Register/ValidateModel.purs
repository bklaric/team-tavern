module TeamTavern.Player.Register.ValidateModel where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Player.Domain.Email (Email, EmailError)
import TeamTavern.Player.Domain.Email as Email
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Password (Password, PasswordError)
import TeamTavern.Player.Domain.Password as Password
import TeamTavern.Player.Register.ReadDto (RegisterDto)

type RegisterModel =
    { email :: Email
    , nickname :: Nickname
    , password :: Password
    }

type RegisterModelError = Variant
    ( email :: NonEmptyList EmailError
    , nickname :: NonEmptyList NicknameError
    , password :: NonEmptyList PasswordError
    )

type ValidateModelError errors = Variant
    ( invalidModel ::
        { dto :: RegisterDto
        , errors :: NonEmptyList RegisterModelError
        }
    | errors )

validateModel :: forall errors.
    RegisterDto -> Async (ValidateModelError errors) RegisterModel
validateModel dto @ { email, nickname, password } = do
    { email: _, nickname: _, password: _ }
        <$> (Email.create email
            # Validated.label (SProxy :: SProxy "email"))
        <*> (Nickname.create nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (Password.create password
            # Validated.label (SProxy :: SProxy "password"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
