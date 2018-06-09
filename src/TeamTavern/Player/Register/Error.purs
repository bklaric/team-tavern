module TeamTavern.Player.Register.Error where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Newtype (unwrap)
import Data.Variant (Variant, match)
import Effect (Effect)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Player.Register.AddPlayer (DatabaseError)
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
