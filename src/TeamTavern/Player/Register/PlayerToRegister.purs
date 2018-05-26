module TeamTavern.Player.Register.PlayerToRegister where

import Prelude

import TeamTavern.Architecture.Async as Async
import TeamTavern.Architecture.Validated as Validated
import Async (Async, fromEither)
import Data.List (List)
import Data.Variant (SProxy(SProxy), Variant)
import Node.Errors (Error)
import TeamTavern.Player.Email (Email, EmailErrors)
import TeamTavern.Player.Email as Email
import TeamTavern.Player.Nickname (Nickname, NicknameErrors)
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

type ValidationErrors = List (Variant
    ( email :: EmailErrors
    , nickname :: NicknameErrors
    ))

type PlayerToRegisterErrors errors = Variant
    ( validation :: ValidationErrors
    , token :: Error
    | errors)

validateEmail :: String -> Validated ValidationErrors Email
validateEmail email =
    Email.create email # Validated.label (SProxy :: SProxy "email")

validateNickname :: String -> Validated ValidationErrors Nickname
validateNickname nickname =
    Nickname.create nickname # Validated.label (SProxy :: SProxy "nickname")

create
    :: forall fields errors
    .  PlayerToRegisterModel fields
    -> Async (PlayerToRegisterErrors errors) PlayerToRegister
create { email, nickname } = do
    token <- Token.create # Async.label (SProxy :: SProxy "token")
    { email: _, nickname: _, token }
        <$> validateEmail email
        <*> validateNickname nickname
        # toEither
        # fromEither
        # Async.label (SProxy :: SProxy "validation")
