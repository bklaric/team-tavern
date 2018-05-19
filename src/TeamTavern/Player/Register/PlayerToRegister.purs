module TeamTavern.Player.Register.PlayerToRegister where

import Prelude

import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors (Error)
import TeamTavern.Architecture (ContV)
import TeamTavern.Player.Email (Email, EmailErrors)
import TeamTavern.Player.Email as Email
import TeamTavern.Player.Nickname (Nickname, NicknameErrors)
import TeamTavern.Player.Nickname as Nickname
import TeamTavern.Player.Token (Token)
import TeamTavern.Player.Token as Token

type PlayerToRegisterModel fields =
    { email :: String
    , nickname :: String
    | fields
    }

type PlayerToRegister =
    { email :: Email
    , nickname :: Nickname
    , token :: Token
    }

type PlayerToRegisterErrors errors = Array (Variant
    ( email :: EmailErrors
    , nickname :: NicknameErrors
    , token :: Error
    | errors))

validateEmail :: forall errors.
    String -> V (Array (Variant (email :: EmailErrors | errors))) Email
validateEmail email =
    Email.create email
    # lmap (inj (SProxy :: SProxy "email") >>> singleton)

validateNickname
    :: forall errors
    .  String
    -> V (Array (Variant (nickname :: NicknameErrors | errors))) Nickname
validateNickname nickname =
    Nickname.create nickname
    # lmap (inj (SProxy :: SProxy "nickname") >>> singleton)

create
    :: forall fields errors
    .  PlayerToRegisterModel fields
    -> ContV (PlayerToRegisterErrors errors) PlayerToRegister
create { email, nickname } = do
    Token.create
    <#>
    case _ of
    Left error -> invalid $ singleton $ inj (SProxy :: SProxy "token") error
    Right token ->
        { email: _, nickname: _, token }
        <$> validateEmail email
        <*> validateNickname nickname
