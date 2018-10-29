module TeamTavern.Player.View.LogError where

import Prelude

import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Postgres.Result (rows)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Player.View.Types (ViewError)

logError :: ViewError -> Effect Unit
logError viewError = do
    log "Error viewing player"
    viewError # match
        { invalidNickname: \{ nickname, errors } -> do
            logt $ "Couldn't validate nickname: " <> show nickname
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableResult: \{ result, errors} -> do
            logt $ "Couldn't read views: " <> (unsafeStringify $ rows result)
            logt $ "Reading views resulted in these errors: " <> show errors
        , notFound: \nickname ->
            logt $ "Player wasn't found: " <> show nickname
        , invalidView: \{ nickname, view } ->
            logt $ "View of player " <> show nickname <> " is invalid: "
                <> show view
        }
