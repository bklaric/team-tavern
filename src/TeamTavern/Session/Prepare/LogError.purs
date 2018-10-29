module TeamTavern.Session.Prepare.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Session.Prepare.Types (PrepareError)

logError :: PrepareError -> Effect Unit
logError prepareError = do
    log "Error preparing session"
    prepareError # match
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
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unknownIdentifiers: \identifiers ->
            logt $ "User supplied identifiers are unknown: " <> show identifiers
        , sendEmailError: \{ message, error } -> do
            logt $ "Couldn't send email message: " <> show message
            logt $ "Email sending resulted in this error: "
                <> show error.status <> ", " <> show error.code <> ", "
                <> error.message
        }
