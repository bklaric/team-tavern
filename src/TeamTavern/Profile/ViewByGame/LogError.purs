module TeamTavern.Profile.ViewByGame.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Profile.ViewByGame.Types (ViewByGameError)

logError :: ViewByGameError -> Effect Unit
logError viewError = do
    log "Error viewing profiles by game"
    viewError # match
        { invalidHandle: \{ handle, errors } -> do
            logt $ "Couldn't validate handle: " <> show handle
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableResult: \{ rows, errors } -> do
            logt $ "Couldn't read profiles out of rows: "
                <> unsafeStringify rows
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidViews: \views -> do
            logt $ "Couldn't validate profiles: " <> show views
        }
