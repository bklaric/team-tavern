module TeamTavern.Player.Register.ErrorModel where

import Prelude

import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Email (EmailError)
import TeamTavern.Player.Nickname (NicknameError)
import TeamTavern.Player.Register.Error (RegisterError)

type RegisterErrorModel = Variant
    ( ensureNotSignedIn :: {}
    , validation :: Array (Variant
        ( email ∷ Array EmailError
        , nickname ∷ Array NicknameError
        ))
    , emailTaken :: {}
    , nicknameTaken :: {}
    , sendEmail :: { credentials :: Credentials }
    , other :: {}
    )

_ensureNotSignedIn = SProxy :: SProxy "ensureNotSignedIn"

_validation = SProxy :: SProxy "validation"

_email = SProxy :: SProxy "email"

_nickname = SProxy :: SProxy "nickname"

_emailTaken = SProxy :: SProxy "emailTaken"

_nicknameTaken = SProxy :: SProxy "nicknameTaken"

_sendEmail = SProxy :: SProxy "sendEmail"

_other = SProxy :: SProxy "other"

fromRegisterPlayerErrors :: RegisterError -> RegisterErrorModel
fromRegisterPlayerErrors = match
    { ensureNotSignedIn: const $ inj _ensureNotSignedIn {}
    , readIdentifiers: const $ inj _other {}
    , validateIdentifiers: _.errors
        >>> fromFoldable
        >>> map (match
            { email: fromFoldable >>> inj _email
            , nickname: fromFoldable >>> inj _nickname
            })
        >>> inj _validation
    , generateToken: const $ inj _other {}
    , addPlayer: match
        { emailTaken: const $ inj _emailTaken {}
        , nicknameTaken: const $ inj _nicknameTaken {}
        , other: const $ inj _other {}
        }
    , sendEmail: _.credentials >>> { credentials: _ } >>> inj _sendEmail
    }
