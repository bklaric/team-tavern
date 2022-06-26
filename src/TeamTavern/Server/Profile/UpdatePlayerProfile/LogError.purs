module TeamTavern.Server.Profile.UpdatePlayerProfile.LogError where

import Prelude

import Data.Array as Array
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect, foreachE)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logLines, logStamped, logt, print)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (ContactsErrors)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (ProfileErrors)
import Type.Function (type ($))
import Yoga.JSON (unsafeStringify)

type UpdateError = Variant
    -- Cookies.
    ( cookieInfoNotPresent :: Map String String
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
    , invalidBody :: NonEmptyList $ Variant
        -- Validate profile.
        ( playerProfile :: ProfileErrors
        -- Validate contacts.
        , playerContacts :: ContactsErrors
        )
    -- Insert profile into database.
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers :: AddPlayerProfile.RouteParams
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
    , internal :: Array String
    , client :: Array String
    , profile :: ProfileErrors
    )

logError :: UpdateError -> Effect Unit
logError updateError = do
    logStamped "Error updating player profile"
    updateError # match
        { cookieInfoNotPresent: \cookies ->
            logt $ "Couldn't read info from cookies: " <> show cookies
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableFields: \{ result, errors } -> do
            logt $ "Couldn't read dto from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading dto resulted in these errors: " <> show errors
        , unreadableProfile: \{ content, errors } -> do
            logt $ "Couldn't read dto out of content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidBody: \errors' -> foreachE (Array.fromFoldable errors') $ match
            { playerProfile: \errors ->
                logt $ "Validation resulted in these errors: " <> show errors
            , playerContacts: \errors -> foreachE (Array.fromFoldable errors) $ match
                { discordTag: logt
                , steamId: logt
                , riotId: logt
                , battleTag: logt
                , eaId: logt
                , psnId: logt
                , gamerTag: logt
                , friendCode: logt
                }
            }
        , notAuthorized: \{ cookieInfo, identifiers } -> do
            logt $ "Player with cookie info: " <> show cookieInfo
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
        , internal: logLines
        , client: logLines
        , profile: \errors ->
            logt $ "Error validating profile: " <> show errors
        }
