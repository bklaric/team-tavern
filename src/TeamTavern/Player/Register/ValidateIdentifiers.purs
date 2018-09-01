module TeamTavern.Player.Register.ValidateIdentifiers
    ( IdentifiersError
    , ValidateIdentifiersError
    , validateIdentifiers
    ) where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Architecture.Validated as Validated
import TeamTavern.Player.Domain.Email (Email, EmailError)
import TeamTavern.Player.Domain.Email as Email
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Types (Identifiers)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)
import Validated (Validated, toEither)

type IdentifiersError = Variant
    ( email :: NonEmptyList EmailError
    , nickname :: NonEmptyList NicknameError
    )

validateEmail :: String -> Validated (NonEmptyList IdentifiersError) Email
validateEmail email =
    Email.create email # Validated.label (SProxy :: SProxy "email")

validateNickname :: String -> Validated (NonEmptyList IdentifiersError) Nickname
validateNickname nickname =
    Nickname.create nickname # Validated.label (SProxy :: SProxy "nickname")

create ::
    IdentifiersModel -> Validated (NonEmptyList IdentifiersError) Identifiers
create { email, nickname } =
    { email: _, nickname: _ }
    <$> validateEmail email
    <*> validateNickname nickname

type ValidateIdentifiersError =
    { errors :: NonEmptyList IdentifiersError
    , model :: IdentifiersModel
    }

validateIdentifiers
    :: forall errors
    .  IdentifiersModel
    -> Async
        (Variant (validateIdentifiers :: ValidateIdentifiersError | errors))
        Identifiers
validateIdentifiers model =
    create model
    # toEither
    # fromEither
    # lmap { errors: _, model }
    # label (SProxy :: SProxy "validateIdentifiers")
