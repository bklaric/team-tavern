module TeamTavern.Server.Player.ViewSettings.LogError where

import Prelude

import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)

type ViewSettingsError = Variant
    ( noCookieInfo :: { cookies :: Map String String }
    , invalidSession :: { cookieInfo :: CookieInfo }
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
        { noCookieInfo: \{ cookies } ->
            logt $ "No player info present in cookies: " <> show cookies
        , invalidSession: \{ cookieInfo } ->
            logt $ "Player has invalid session info in cookies: "
                <> show cookieInfo
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
