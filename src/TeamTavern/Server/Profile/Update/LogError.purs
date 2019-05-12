module TeamTavern.Server.Profile.Update.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logt, print)
import TeamTavern.Server.Profile.Routes (Identifiers)

type UpdateError = Variant
    ( cookieInfoNotPresent :: Map String String
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidSummary ::
        { summary :: String
        , errors :: NonEmptyList NonEmptyTextError
        }
    , databaseError :: Error
    , notAuthorized ::
        { auth :: CookieInfo
        , identifiers :: Identifiers
        }
    )

logError :: UpdateError -> Effect Unit
logError updateError = do
    log "Error updating profile"
    updateError # match
        { cookieInfoNotPresent: \cookies ->
            logt $ "Couldn't read info from cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read summary out of content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidSummary: \{ summary, errors } -> do
            logt $ "Couldn't validate summary: " <> show summary
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , notAuthorized: \{ auth, identifiers } -> do
            logt $ "Player with auth: " <> show auth
            logt $ "Not authorized to update profile for identifiers: "
                <> show identifiers
        }
