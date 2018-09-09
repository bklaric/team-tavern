module TeamTavern.Session.Start.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Postgres.Result (rows)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Session.Start.Types (StartError)

logError :: StartError -> Effect Unit
logError startError = do
    log "Error starting session"
    startError # match
        { unreadableNicknamedNonce: \{ content, errors } -> do
            logt $ "Couldn't read nicknamed nonce from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidNicknamedNonce: \{ nicknamedNonce, errors } -> do
            logt $ "Couldn't validate nicknamed nonce: " <> show nicknamedNonce
            logt $ "Validation resulted in these errors: " <> show errors
        , noTokenToConsume: \nicknamedNonce ->
            logt $ "No valid token to consume for nicknamed nonce: "
                <> show nicknamedNonce
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableIdentifiedToken: \{ result, errors } -> do
            logt $ "Couldn't read identified token from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidIdentifiedToken: \identifiedToken ->
            logt $ "Couldn't validate identified token from result: "
                <> show identifiedToken
        }
