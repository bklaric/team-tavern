module TeamTavern.Player.Register.Types.Error where

import Prelude

import Effect.Console (log)
import Data.Newtype (unwrap)
import Data.Variant (Variant, match)
import Effect (Effect)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (EnsureNotSignedInError)
import TeamTavern.Player.Register.AddPlayer (AddPlayerError)
import TeamTavern.Player.Register.GenerateToken (GenerateTokenError)
import TeamTavern.Player.Register.ReadIdentifiers (ReadIdentifiersError)
import TeamTavern.Player.Register.NotifyPlayer (SendEmailError)
import TeamTavern.Player.Register.ValidateIdentifiers (ValidateIdentifiersError)
import Unsafe.Coerce (unsafeCoerce)

type RegisterError = Variant
    ( ensureNotSignedIn :: EnsureNotSignedInError
    , readIdentifiers :: ReadIdentifiersError
    , validateIdentifiers :: ValidateIdentifiersError
    , generateToken :: GenerateTokenError
    , addPlayer :: AddPlayerError
    , sendEmail :: SendEmailError
    )

logError :: RegisterError -> Effect Unit
logError registerError = unsafeCoerce do
    log "Error registering player"
    registerError # match
        { ensureNotSignedIn: \{ token } -> do
            log $ "\tThe request came with the following token: " <> token
        , readIdentifiers: \{ errors, body } -> do
            log $ "\tCouldn't read identifiers from body: " <> show body
            log $ "\tParsing resulted in these errors: " <> show errors
        , validateIdentifiers: \{ errors, model } ->
            log $ "\tFailed identifiers validation for identifiers: "
                <> model.email <> ", " <> model.nickname
        , generateToken: match
            { random: \{ error, identifiers } -> do
                log $ "\tCouldn't generate random bytes for identifiers: "
                    <> unwrap identifiers.email <> ", "
                    <> unwrap identifiers.nickname
                log $ "\tGenerating random bytes resulted in this error: "
                    <> code error <> ", " <> name error <> ", "
                    <> message error
            , token: \{ errors, identifiers } -> do
                log $ "\tFailed token validation for identifiers: "
                    <> unwrap identifiers.email <> ", "
                    <> unwrap identifiers.nickname
            , nonce: \{ errors, identifiers } -> do
                log $ "\tFailed nonce validation for identifiers: "
                    <> unwrap identifiers.email <> ", "
                    <> unwrap identifiers.nickname
            }
        , addPlayer: match
            { emailTaken: \{ error, credentials } ->
                log $ "\tEmail is already taken for credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
            , nicknameTaken: \{ error, credentials } ->
                log $ "\tNickname is already taken for credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
            , cantReadPlayerId: \{ result, credentials } -> do
                log $ "\tCouldn't add to database player with credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
                log $ "\tCouldn't read player id from insert result"
            , other: \{ error, credentials } -> do
                log $ "\tCouldn't add to database player with credentials: "
                    <> unwrap credentials.email <> ", "
                    <> unwrap credentials.nickname <> ", "
                    <> unwrap credentials.token
                log $ "\tAdding to database resulted in this error: "
                    <> code error <> ", " <> name error <> ", "
                    <> message error
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
