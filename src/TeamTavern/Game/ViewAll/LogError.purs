module TeamTavern.Game.ViewAll.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Global.Unsafe (unsafeStringify)
import Postgres.Result (rows)
import TeamTavern.Game.ViewAll.Types (ViewAllError)
import TeamTavern.Infrastructure.Log (logt, print)

logError :: ViewAllError -> Effect Unit
logError viewAllError = do
    logt "Error viewing all games"
    viewAllError # match
        { unreadableViews: \{ result, errors } -> do
            logt $ "Couldn't read view from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading views resulted in these errors: " <> show errors
        , invalidViews: \views ->
            logt $ "The following game views are invalid: " <> show views
        , databaseError: \error ->
            logt $ "Database error occured: " <> print error
        }
