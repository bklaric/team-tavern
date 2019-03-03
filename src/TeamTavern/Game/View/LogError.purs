module TeamTavern.Game.View.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Infrastructure.Log (logt, print)

type ViewError = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Handle
    )

logError :: ViewError -> Effect Unit
logError viewError = do
    logt "Error viewing game"
    viewError # match
        { databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        , unreadableDto: \{ result, errors } -> do
            logt $ "Couldn't read dto from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading dto resulted in these errors: " <> show errors
        , notFound: \handle ->
            logt $ "Game '" <> show handle <> "' wasn't found"
        }
