module TeamTavern.Server.Profile.UpdateTeamProfile.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant, match)
import Effect (Effect)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt, print)
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as ValidateProfile
import TeamTavern.Server.Profile.Routes (Handle)

type AddGameTeamError = Variant
    -- Ensure signed in.
    ( noCookieInfo :: { cookies :: Cookies }
    , databaseError :: Error
    , invalidSession :: { cookieInfo :: CookieInfo }
    -- Ensure signed in as.
    , nicknameDoesntMatch ::
        { nickname :: String
        , cookieInfo :: CookieInfo
        }
    -- Load fields from database.
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
    , nothingInserted ::
        { cookieInfo :: CookieInfo
        , handle :: Handle
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

logError :: AddGameTeamError -> Effect Unit
logError addError = do
    logStamped "Error creating profile"
    addError # match
        { noCookieInfo: \{ cookies } ->
            logt $ "No player info present in cookies: " <> show cookies
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , invalidSession: \{ cookieInfo } ->
            logt $ "Player has invalid session info in cookies: "
                <> show cookieInfo
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
        , nothingInserted: \{ cookieInfo, handle } -> do
            logt $ "Player with cookie info: " <> show cookieInfo
            logt $ "Nothing inserted into profile table for game: " <> handle
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
