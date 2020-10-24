module TeamTavern.Server.Player.EditSettings.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)

type EditSettingsError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthorized :: Array String
    , databaseError :: Error
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
        { internal: logLines
        , client: logLines
        , notAuthorized: logLines
        , nicknameDoesntMatch: \{ nickname, cookieInfo } -> do
            logt $ "Signed in user: " <> show cookieInfo
            logt $ "Doesn't have requested nickname: " <> nickname
        , unreadableModel: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        }
