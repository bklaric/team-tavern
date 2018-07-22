module TeamTavern.Player.Register.Types.Identifiers where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Architecture.Validated (label)
import TeamTavern.Player.Domain.Email (Email, EmailError)
import TeamTavern.Player.Domain.Email as Email
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import Validated (Validated)

type IdentifiersModel = { email :: String, nickname :: String }

type Identifiers = { email :: Email, nickname :: Nickname }

type IdentifiersError = Variant
    ( email :: NonEmptyList EmailError
    , nickname :: NonEmptyList NicknameError
    )

validateEmail :: String -> Validated (NonEmptyList IdentifiersError) Email
validateEmail email =
    Email.create email # label (SProxy :: SProxy "email")

validateNickname :: String -> Validated (NonEmptyList IdentifiersError) Nickname
validateNickname nickname =
    Nickname.create nickname # label (SProxy :: SProxy "nickname")

create ::
    IdentifiersModel -> Validated (NonEmptyList IdentifiersError) Identifiers
create { email, nickname } =
    { email: _, nickname: _ }
    <$> validateEmail email
    <*> validateNickname nickname
