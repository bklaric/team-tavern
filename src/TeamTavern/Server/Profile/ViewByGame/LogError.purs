module TeamTavern.Server.Profile.ViewByGame.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)

type ViewAllError = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , unreadableCount ::
        { count :: Foreign
        , errors :: MultipleErrors
        }
    , noRowsSomehow :: Result
    )

logError :: ViewAllError -> Effect Unit
logError viewError = do
    logStamped "Error viewing profiles by game"
    viewError # match
        { databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableDtos: \{ result, errors } -> do
            logt $ "Couldn't read dtos out of rows: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , unreadableCount: \{ count, errors } -> do
            logt $ "Couldn't read profile count from foreign: "
                <> unsafeStringify count
            logt $ "Reading profile count resulted in these errors: "
                <> show errors
        , noRowsSomehow: \result ->
            logt $ "Profile count query somehow returned no rows: "
                <> (unsafeStringify $ rows result)
        }
