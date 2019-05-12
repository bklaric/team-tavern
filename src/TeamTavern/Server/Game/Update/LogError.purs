module TeamTavern.Server.Game.Update.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.Domain.Title (Title)
import TeamTavern.Server.Game.Infrastructure.ReadModel (GameDto, GameModelError)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logt, print)

type UpdateError = Variant
    ( cookieInfoNotPresent :: Map String String
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: GameDto
        , errors :: NonEmptyList GameModelError
        }
    , titleTaken ::
        { title :: Title
        , error :: Error
        }
    , handleTaken ::
        { handle :: Handle
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized :: CookieInfo
    )

logError :: UpdateError -> Effect Unit
logError updateError = do
    logt "Error updating game"
    updateError # match
        { cookieInfoNotPresent: \cookies ->
            logt $ "Couldn't read auth info out of cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto from content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidModel: \{ dto, errors } -> do
            logt $ "Couldn't validate model: " <> show dto
            logt $ "Validating resulted in these errors: " <> show errors
        , titleTaken: \{ title, error } -> do
            logt $ "Title is already taken: " <> show title
            logt $ "According to this error: " <> print error
        , handleTaken: \{ handle, error } -> do
            logt $ "Handle is already taken: " <> show handle
            logt $ "According to this error: " <> print error
        , databaseError: \error ->
            logt $ "Unknown database error occured: " <> print error
        , notAuthorized: \authInfo ->
            logt $ "Game creation isn't authorized for this player info: "
                <> show authInfo
        }
