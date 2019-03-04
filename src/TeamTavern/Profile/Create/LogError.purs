module TeamTavern.Profile.Create.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Profile.Domain.Summary (SummaryError)

type CreateError = Variant
    ( cookieInfoNotPresent :: Map String String
    , unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidSummary ::
        { summary :: String
        , errors :: NonEmptyList SummaryError
        }
    , databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , handle :: Handle
        }
    )

logError :: CreateError -> Effect Unit
logError createError = do
    log "Error creating profile"
    createError # match
        { cookieInfoNotPresent: \cookies ->
            logt $ "Couldn't read info from cookies: " <> show cookies
        , unreadableDto: \{ content, errors } -> do
            logt $ "Couldn't read dto out of content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidSummary: \{ summary, errors } -> do
            logt $ "Couldn't validate summary: " <> show summary
            logt $ "Validation resulted in these errors: " <> show errors
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , notAuthorized: \{ cookieInfo, handle } -> do
            logt $ "Player with cookie info: " <> show cookieInfo
            logt $ "Not authorized to create profile for handle: "
                <> show handle
        }
