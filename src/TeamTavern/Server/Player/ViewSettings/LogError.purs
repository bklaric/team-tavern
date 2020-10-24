module TeamTavern.Server.Player.ViewSettings.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)

type ViewSettingsError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthorized :: Array String
    , nicknameDoesntMatch ::
        { nickname :: String
        , cookieInfo :: CookieInfo
        }
    , notFound :: String
    , unreadableSettings ::
        { settings :: Foreign
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    )

logError :: ViewSettingsError -> Effect Unit
logError viewError = do
    logStamped "Error viewing player account"
    viewError # match
        { internal: logLines
        , client: logLines
        , notAuthorized: logLines
        , nicknameDoesntMatch: \{ nickname, cookieInfo } -> do
            logt $ "Signed in user: " <> show cookieInfo
            logt $ "Doesn't have requested nickname: " <> nickname
        , notFound: \nickname ->
            logt $ "No account found for nickname: " <> nickname
        , unreadableSettings: \{ settings, errors } -> do
            logt $ "Couldn't read settings: " <> (unsafeStringify settings)
            logt $ "Reading settings resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        }
