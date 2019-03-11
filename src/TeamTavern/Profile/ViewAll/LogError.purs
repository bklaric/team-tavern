module TeamTavern.Profile.ViewByGame.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Infrastructure.Log (logt, print)

type ViewAllError = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    )

logError :: ViewAllError -> Effect Unit
logError viewError = do
    log "Error viewing profiles by game"
    viewError # match
        { databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableDtos: \{ result, errors } -> do
            logt $ "Couldn't read dtos out of rows: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        }
