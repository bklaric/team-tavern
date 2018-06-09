module TeamTavern.Player.Register.Errors where

import Prelude

import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Email (EmailError)
import TeamTavern.Player.Nickname (NicknameError)
import TeamTavern.Player.Register.Database (DatabaseError)
import TeamTavern.Player.Register.Identifiers (ModelError, ValidationError)
import TeamTavern.Player.Register.SendEmail (SendEmailError)
import TeamTavern.Player.Register.Token (TokenError)

type RegisterError = Variant
    ( model :: ModelError
    , validation :: ValidationError
    , token :: TokenError
    , database :: DatabaseError
    , sendEmail :: SendEmailError
    )

type RegisterPlayerErrorModel = Variant
    ( validation :: Array (Variant
        ( email ∷ Array EmailError
        , nickname ∷ Array NicknameError
        ))
    , emailTaken :: {}
    , nicknameTaken :: {}
    , sendEmail :: { credentials :: Credentials }
    , other :: {}
    )

_validation = SProxy :: SProxy "validation"

_email = SProxy :: SProxy "email"

_nickname = SProxy :: SProxy "nickname"

_emailTaken = SProxy :: SProxy "emailTaken"

_nicknameTaken = SProxy :: SProxy "nicknameTaken"

_sendEmail = SProxy :: SProxy "sendEmail"

_other = SProxy :: SProxy "other"

fromRegisterPlayerErrors :: RegisterError -> RegisterPlayerErrorModel
fromRegisterPlayerErrors = match
    { model: const $ inj _other {}
    , validation: _.errors
        >>> fromFoldable
        >>> map (match
            { email: fromFoldable >>> inj _email
            , nickname: fromFoldable >>> inj _nickname
            })
        >>> inj _validation
    , token: const $ inj _other {}
    , database: match
        { emailTaken: const $ inj _emailTaken {}
        , nicknameTaken: const $ inj _nicknameTaken {}
        , other: const $ inj _other {}
        }
    , sendEmail: _.credentials >>> { credentials: _ } >>> inj _sendEmail
    }
