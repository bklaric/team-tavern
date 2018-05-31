module TeamTavern.Player.Register.Errors where

import Prelude

import Data.Array (fromFoldable)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant, inj, match)
import Node.Errors as Node
import Postmark.Error as Postmark
import TeamTavern.Player.Email (EmailError)
import TeamTavern.Player.Identifiers (IdentifiersError)
import TeamTavern.Player.Nickname (NicknameError)
import TeamTavern.Player.Register.Database (DatabaseError)

type RegisterError = Variant
    ( model :: NonEmptyList ForeignError
    , identifiers :: NonEmptyList IdentifiersError
    , token :: Node.Error
    , database :: DatabaseError
    , email :: Postmark.Error
    )

type RegisterPlayerErrorModel = Variant
    ( identifiers :: Array (Variant
        ( email ∷ Array EmailError
        , nickname ∷ Array NicknameError
        ))
    , emailTaken :: {}
    , nicknameTaken :: {}
    , other :: {}
    )

_identifiers = SProxy :: SProxy "identifiers"

_email = SProxy :: SProxy "email"

_nickname = SProxy :: SProxy "nickname"

_emailTaken = SProxy :: SProxy "emailTaken"

_nicknameTaken = SProxy :: SProxy "nicknameTaken"

_other = SProxy :: SProxy "other"

fromRegisterPlayerErrors :: RegisterError -> RegisterPlayerErrorModel
fromRegisterPlayerErrors = match
    { model: const $ inj _other {}
    , identifiers: fromFoldable
        >>> map (match
            { email: fromFoldable >>> inj _email
            , nickname: fromFoldable >>> inj _nickname
            })
        >>> inj _identifiers
    , token: const $ inj _other {}
    , database: match
        { emailTaken: const $ inj _emailTaken {}
        , nicknameTaken: const $ inj _nicknameTaken {}
        , other: const $ inj _other {}
        }
    , email: const $ inj _other {}
    }
