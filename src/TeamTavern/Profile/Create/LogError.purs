module TeamTavern.Profile.Create.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Profile.Create.Types (CreateError)

logError :: CreateError -> Effect Unit
logError createError = do
    log "Error creating profile"
    createError # match
        { invalidHandle: \{ handle, errors } -> do
            logt $ "Couldn't validate handle: " <> show handle
            logt $ "Validation resulted in these errors: " <> show errors
        , authNotPresent: \cookies ->
            logt $ "Couldn't read auth info from cookies: " <> show cookies
        , unreadableSummary: \{ content, errors } -> do
            logt $ "Couldn't read summary out of content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidSummary: \{ summary, errors } -> do
            logt $ "Couldn't validate summary: " <> show summary
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , notAuthorized: \{ auth, handle } -> do
            logt $ "Player with auth: " <> show auth
            logt $ "Not authorized to create profile for handle: "
                <> show handle
        }
