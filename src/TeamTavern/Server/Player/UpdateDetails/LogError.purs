module TeamTavern.Server.Player.UpdateDetails.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)
import TeamTavern.Server.Player.UpdateDetails.ReadUpdate (UpdateDetailsDto, UpdateDetailsModelError)

type UpdateDetailsError = Variant
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
        { dto :: UpdateDetailsDto
        , errors :: NonEmptyList UpdateDetailsModelError
        }
    )

logError :: UpdateDetailsError -> Effect Unit
logError updateError = do
    logStamped "Error updating player"
    updateError # match
        { internal: logLines
        , client: logLines
        , notAuthorized: logLines
        , nicknameDoesntMatch: \{ nickname, cookieInfo } -> do
            logt $ "Signed in user: " <> show cookieInfo
            logt $ "Doesn't have requested nickname: " <> nickname
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from body: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidModel: \{ dto, errors } -> do
            logt $ "Couldn't validate model from dto: " <> show dto
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        }
