module TeamTavern.Player.Register.Run.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import TeamTavern.Player.Register.Run.Types (RegisterError)

logError :: RegisterError -> Effect Unit
logError registerError = do
    log "Error registering player"
    registerError # match
        { ensureNotSignedIn: \{ playerId } ->
            log $ "\tThe request came with this player id: " <> playerId
        , readIdentifiers: \{ errors, body } -> do
            log $ "\tCouldn't read identifiers from body: " <> show body
            log $ "\tParsing resulted in these errors: " <> show errors
        , validateIdentifiers: \{ errors, model } ->
            log $ "\tFailed identifiers validation for identifiers: "
                <> model.email <> ", " <> model.nickname
        , generateSecrets: \{ error, identifiers} -> do
            log $ "\tCouldn't generate secret for identifiers: "
                <> unwrap identifiers.email <> ", "
                <> unwrap identifiers.nickname
            error # match
                { random: \error' ->
                    log $ "\tGenerating random bytes resulted in this error: "
                        <> code error' <> ", " <> name error' <> ", "
                        <> message error'
                , token: \errors -> do
                    log $ "\tToken validation failed with these errors: "
                        <> show errors
                , nonce: \errors -> do
                    log $ "\tNonce validation failed with these errors: "
                        <> show errors
                }
        , addPlayer: \{ credentials, error } -> do
            log $ "\tCouldn't add to database player with credentials: "
                <> unwrap credentials.email <> ", "
                <> unwrap credentials.nickname <> ", "
                <> unwrap credentials.token <> ", "
                <> unwrap credentials.nonce
            error # match
                { emailTaken: \error' ->
                    log $ "\tEmail is already taken: "
                        <> code error' <> ", " <> name error' <> ", "
                        <> message error'
                , nicknameTaken: \error' ->
                    log $ "\tNickname is already taken: "
                        <> code error' <> ", " <> name error' <> ", "
                        <> message error'
                , other: \error' -> do
                    log $ "\tAdding to database resulted in this error: "
                        <> code error' <> ", " <> name error' <> ", "
                        <> message error'
                }
        , sendEmail: \{ error, identifiers } -> do
            log $ "\tCouldn't send email to player with nonced identifiers: "
                <> unwrap identifiers.email <> ", "
                <> unwrap identifiers.nickname <> ", "
                <> unwrap identifiers.nonce
            log $ "\tEmail sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }
