module TeamTavern.Server.Player.ChangeNickname.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)
import TeamTavern.Server.Player.ChangeNickname.ReadNickname (ChangeNicknameDto)
import TeamTavern.Server.Player.Domain.Nickname (Nickname, NicknameError)

type UpdateError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthorized :: Array String
    , databaseError :: Error
    , nicknameDoesntMatch :: { nickname :: String, cookieInfo :: CookieInfo }
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: ChangeNicknameDto
        , errors :: NonEmptyList NicknameError
        }
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Error
        }
    )

logError :: UpdateError -> Effect Unit
logError updateError = do
    logStamped "Error updating player"
    updateError # match
        { internal: logLines
        , client: logLines
        , nicknameDoesntMatch: \{ nickname, cookieInfo } -> do
            logt $ "Signed in user: " <> show cookieInfo
            logt $ "Doesn't have requested nickname: " <> nickname
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidModel: \{ dto, errors } -> do
            logt $ "Couldn't validate model from dto: " <> show dto
            logt $ "Validation resulted in these errors: " <> show errors
        , nicknameTaken: \{ nickname, error } -> do
            logt $ "Nickname is already taken: " <> show nickname
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , notAuthorized: logLines
        }
