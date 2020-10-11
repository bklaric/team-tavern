module TeamTavern.Server.Profile.AddPlayerProfile.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile as ValidateProfile
import TeamTavern.Server.Profile.Routes (Identifiers)

type CreateError = Variant
    -- Cookies.
    ( internal :: Array String
    , client :: Array String
    , nicknameDoesntMatch ::
        { cookieInfo :: CookieInfo
        , nickname :: String
        }
    -- Load fields from database.
    , databaseError :: Error
    , unreadableFields ::
        { result :: Result
        , errors :: MultipleErrors
        }
    -- Read profile from body.
    , unreadableProfile ::
        { content :: String
        , errors :: MultipleErrors
        }
    -- Validate profile.
    , invalidProfile ::
        { profile :: ReadProfile.Profile
        , errors :: NonEmptyList ValidateProfile.ProfileError
        }
    -- Insert profile into database.
    , notAuthorized ::
        { playerId :: Int
        , identifiers :: Identifiers
        }
    , unreadableProfileId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , emptyResult ::
        { result :: Result
        }
    , unreadableFieldValueId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    )

logError :: CreateError -> Effect Unit
logError createError = do
    logStamped "Error creating profile"
    createError # match
        { internal: logLines
        , client: logLines
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , nicknameDoesntMatch: \{ nickname, cookieInfo } -> do
            logt $ "Signed in user: " <> show cookieInfo
            logt $ "Doesn't have requested nickname: " <> nickname
        , unreadableFields: \{ result, errors } -> do
            logt $ "Couldn't read dto from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading dto resulted in these errors: " <> show errors
        , unreadableProfile: \{ content, errors } -> do
            logt $ "Couldn't read dto out of content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidProfile: \{ profile, errors } -> do
            logt $ "Couldn't validate profile: " <> show profile
            logt $ "Validation resulted in these errors: " <> show errors
        , notAuthorized: \{ playerId, identifiers } -> do
            logt $ "Player with id: " <> show playerId
            logt $ "Not authorized to create profile for identifiers: "
                <> show identifiers
        , unreadableProfileId: \{ result, errors } -> do
            logt $ "Couldn't read profile id from insert result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        , emptyResult: \{ result } -> do
            logt $ "Expected at least one row as insert result, got none: "
                <> (unsafeStringify $ rows result)
        , unreadableFieldValueId: \{ result, errors } -> do
            logt $ "Couldn't read profile id from insert result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        }
