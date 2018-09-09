module TeamTavern.Player.Register.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Player.Register.Types (RegisterError)

logError :: RegisterError -> Effect Unit
logError registerError = do
    log "Error registering player"
    registerError # match
        { signedIn: \{ playerId, cookies } -> do
            logt $ "The request came with this player id: " <> show playerId
            logt $ "In these cookies: " <> show cookies
        , unreadableIdentifiers: \{ content, errors } -> do
            logt $ "Couldn't read identifiers from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidIdentifiers: \{ identifiers, errors } -> do
            logt $ "Couldn't validate identifiers: " <> show identifiers
            logt $ "Validation resulted in these errors: " <> show errors
        , randomError: \error ->
            logt $ "Generating random bytes resulted in this error: "
                <> print error
        , invalidGeneratedNonce: \{ nonce, errors } -> do
            logt $ "Couldn't validate generated nonce: " <> show nonce
            logt $ "Validation resulted in these errors: " <> show errors
        , invalidGeneratedToken: \{ token, errors } -> do
            logt $ "Couldn't validate generated token: " <> show token
            logt $ "Validation resulted in these errors: " <> show errors
        , emailTaken: \{ email, error } -> do
            logt $ "Email is already taken: " <> show email
            logt $ "According to this error: " <> print error
        , nicknameTaken: \{ nickname, error } -> do
            logt $ "Nickname is already taken: " <> show nickname
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , sendEmailError: \{ identifiers, message, error } -> do
            logt $ "Couldn't send email to player: " <> show identifiers
            logt $ "Email message: " <> show message
            logt $ "Email sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }
