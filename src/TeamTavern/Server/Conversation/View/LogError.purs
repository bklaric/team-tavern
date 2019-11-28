module TeamTavern.Server.Conversation.View.LogError where

import Prelude

import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)

type ViewError = Variant
    ( noCookieInfo :: { cookies :: Map String String }
    , invalidSession :: { cookieInfo :: CookieInfo }
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
        { noCookieInfo: \{ cookies } ->
            logt $ "No player info present in cookies: " <> show cookies
        , invalidSession: \{ cookieInfo } ->
            logt $ "Player has invalid session info in cookies: " <> show cookieInfo
        , unreadableResult: \{ result, errors } -> do
            logt $ "Couldn't read conversation from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading conversation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        }
