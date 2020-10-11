module TeamTavern.Server.Conversation.ViewAll.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)

type ViewAllError = Variant
    ( internal :: Array String
    , client :: Array String
    , unreadableResult ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    )

logError :: ViewAllError -> Effect Unit
logError viewAllError = do
    logStamped "Error viewing all conversations"
    viewAllError # match
        { internal: logLines
        , client: logLines
        , unreadableResult: \{ result, errors } -> do
            logt $ "Couldn't read conversations from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading conversations resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        }
