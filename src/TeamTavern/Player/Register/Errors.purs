module TeamTavern.Player.Register.Errors where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array (fromFoldable)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj, match)
import Effect (Effect)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Email (EmailError)
import TeamTavern.Player.Nickname (NicknameError)
import TeamTavern.Player.Register.Database (DatabaseError)
import TeamTavern.Player.Register.Identifiers (ModelError, ValidationError)
import TeamTavern.Player.Register.SendEmail (SendEmailError)
import TeamTavern.Player.Register.Token (TokenError)
import Unsafe.Coerce (unsafeCoerce)

type RegisterError = Variant
    ( model :: ModelError
    , validation :: ValidationError
    , token :: TokenError
    , database :: DatabaseError
    , sendEmail :: SendEmailError
    )

logError :: RegisterError -> Effect Unit
logError registerError = unsafeCoerce do
    log "Error registering player"
    registerError # match
        { model: \{ errors, body } -> do
            log $ "\tCouldn't read identifiers from body: " <> show body
            log $ "\tParsing resulted in these errors: " <> show errors
        , validation: \{ errors, model } ->
            log $ "\tFailed identifiers validation for identifiers: "
                <> model.email <> ", " <> model.nickname
        , token: \{ error, identifiers } -> do
            log $ "\tCouldn't generate token for identifiers: "
                <> unwrap identifiers.email <> ", "
                <> unwrap identifiers.nickname
            log $ "\tGenerating token resulted in this error: "
                <> code error <> ", " <> name error <> ", "
                <> message error
        , database: match
            { other: \{ error, credentials } -> do
                log $ "\tCouldn't add to database player with credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
                log $ "\tAdding to database resulted in this error: "
                    <> code error <> ", " <> name error <> ", "
                    <> message error
            , emailTaken: \{ error, credentials } ->
                log $ "\tEmail is already taken for credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
            , nicknameTaken: \{ error, credentials } ->
                log $ "\tNickname is already taken for credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
            }
        , sendEmail: \{ error, credentials } -> do
            log $ "\tCouldn't send email to player with credentials: "
                <> unwrap credentials.email <> ", "
                <> unwrap credentials.nickname <> ", "
                <> unwrap credentials.token
            log $ "\tEmail sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }

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
