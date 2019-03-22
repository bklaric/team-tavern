module TeamTavern.Game.ViewAll.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Infrastructure.Log (logt, print)

type ViewAllError = Variant
    ( unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    )

logError :: ViewAllError -> Effect Unit
logError viewAllError = do
    logt "Error viewing all games"
    viewAllError # match
        { unreadableDtos: \{ result, errors } -> do
            logt $ "Couldn't read game dtos from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading dtos resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        }
