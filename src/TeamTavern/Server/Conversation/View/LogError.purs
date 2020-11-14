module TeamTavern.Server.Conversation.View.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)

type ViewError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthenticated :: Array String
    , unreadableResult ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    )

logError :: ViewError -> Effect Unit
logError viewError = do
    logStamped "Error viewing conversation"
    viewError # match
        { internal: logLines
        , client: logLines
        , notAuthenticated: logLines
        , unreadableResult: \{ result, errors } -> do
            logt $ "Couldn't read conversation from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading conversation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        }
