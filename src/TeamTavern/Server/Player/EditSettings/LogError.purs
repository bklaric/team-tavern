module TeamTavern.Server.Player.EditSettings.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)

type EditSettingsError = Variant
    ( noCookieInfo :: { cookies :: Cookies }
    , databaseError :: Error
    , invalidSession :: { cookieInfo :: CookieInfo }
    , nicknameDoesntMatch :: { nickname :: String, cookieInfo :: CookieInfo }
    , unreadableModel ::
        { content :: String
        , errors :: MultipleErrors
        }
    )

logError :: EditSettingsError -> Effect Unit
logError updateError = do
    logStamped "Error updating player"
    updateError # match
        { noCookieInfo: \{ cookies } ->
            logt $ "No player info present in cookies: " <> show cookies
        , invalidSession: \{ cookieInfo } ->
            logt $ "Player has invalid session info in cookies: "
                <> show cookieInfo
        , nicknameDoesntMatch: \{ nickname, cookieInfo } -> do
            logt $ "Signed in user: " <> show cookieInfo
            logt $ "Doesn't have requested nickname: " <> nickname
        , unreadableModel: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        }
