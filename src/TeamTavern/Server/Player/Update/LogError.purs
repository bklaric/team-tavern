module TeamTavern.Server.Player.Update.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Update.ReadUpdate (UpdateDto, UpdateModelError)

type UpdateError = Variant
    ( cookieInfoNotPresent :: Map String String
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: UpdateDto
        , errors :: NonEmptyList UpdateModelError
        }
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , nickname :: Nickname
        }
    )

logError :: UpdateError -> Effect Unit
logError updateError = do
    logStamped "Error updating player"
    updateError # match
        { cookieInfoNotPresent: \cookies ->
            logt $ "Couldn't read player info out of cookies: " <> show cookies
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
        , notAuthorized: \{ cookieInfo, nickname } -> do
            logt $ "Player with cookie info: " <> show cookieInfo
            logt $ "Not authorized to update player: " <> show nickname
        }
