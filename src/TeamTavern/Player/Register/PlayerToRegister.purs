module TeamTavern.Player.Register.PlayerToRegister where

import Prelude

import Async (Async, fromEither)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(SProxy), Variant)
import Node.Errors (Error)
import TeamTavern.Architecture.Async as Async
import TeamTavern.Architecture.Validated as Validated
import TeamTavern.Player.Email (Email, EmailError)
import TeamTavern.Player.Email as Email
import TeamTavern.Player.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Nickname as Nickname
import TeamTavern.Player.Register.PlayerToRegisterModel (PlayerToRegisterModel)
import TeamTavern.Player.Token (Token)
import TeamTavern.Player.Token as Token
import Validated (Validated, toEither)

type PlayerToRegister =
    { email :: Email
    , nickname :: Nickname
    , token :: Token
    }

type ValidationError = Variant
    ( email :: NonEmptyList EmailError
    , nickname :: NonEmptyList NicknameError
    )

type PlayerToRegisterError errors = Variant
    ( validation :: NonEmptyList ValidationError
    , token :: Error
    | errors)

validateEmail :: String -> Validated (NonEmptyList ValidationError) Email
validateEmail email =
    Email.create email # Validated.label (SProxy :: SProxy "email")

validateNickname :: String -> Validated (NonEmptyList ValidationError) Nickname
validateNickname nickname =
    Nickname.create nickname # Validated.label (SProxy :: SProxy "nickname")

create
    :: forall fields errors
    .  PlayerToRegisterModel fields
    -> Async (PlayerToRegisterError errors) PlayerToRegister
create { email, nickname } = do
    token <- Token.create # Async.label (SProxy :: SProxy "token")
    { email: _, nickname: _, token }
        <$> validateEmail email
        <*> validateNickname nickname
        # toEither
        # fromEither
        # Async.label (SProxy :: SProxy "validation")
