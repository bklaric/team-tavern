module TeamTavern.Game.View.LogError where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant (match)
import Effect (Effect)
import Global.Unsafe (unsafeStringify)
import Postgres.Result (rows)
import TeamTavern.Game.View.Types (ViewError)
import TeamTavern.Infrastructure.Log (logt, print)

logError :: ViewError -> Effect Unit
logError viewError = do
    logt "Error viewing game"
    viewError # match
        { invalidHandle: \{ handle, errors } -> do
            logt $ "Couldn't validate handle: " <> show handle
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        , unreadableView: \{ result, errors } -> do
            logt $ "Couldn't read view from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading view resulted in these errors: " <> show errors
        , notFound: \handle ->
            logt $ "Game '" <> unwrap handle <> "' wasn't found"
        , invalidView: \{ handle, view } -> do
            logt $ "Couldn't validate view of game with handle: " <> show handle
            logt $ "View from result: " <> show view
        }
