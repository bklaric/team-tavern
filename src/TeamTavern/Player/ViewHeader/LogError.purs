module TeamTavern.Player.ViewHeader.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Player.Domain.Id (Id)

type ViewHeaderError = Variant
    ( databaseError :: Error
    , unreadableHeader ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Id
    )

logError :: ViewHeaderError -> Effect Unit
logError registerError = do
    log "Error registering player"
    registerError # match
        { databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableHeader: \{ result, errors } -> do
            logt $ "Can't read player header from from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , notFound: \id ->
            logt $ "Player wasn't found: " <> show id
        }
