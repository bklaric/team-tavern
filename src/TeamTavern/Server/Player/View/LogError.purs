module TeamTavern.Server.Player.View.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Log (logt, print)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type ViewError = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Nickname
    )

logError :: ViewError -> Effect Unit
logError viewError = do
    log "Error viewing player"
    viewError # match
        { databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableDto: \{ result, errors} -> do
            logt $ "Couldn't read dtos from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading dtos resulted in these errors: " <> show errors
        , notFound: \nickname ->
            logt $ "Player wasn't found: " <> show nickname
        }
