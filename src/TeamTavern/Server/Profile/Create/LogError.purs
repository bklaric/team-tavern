module TeamTavern.Server.Profile.Create.LogError where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Log (logt, print)
import TeamTavern.Server.Profile.Infrastructure.LoadFields as LoadFields
import TeamTavern.Server.Profile.Infrastructure.ReadProfile (ProfileDto, ProfileModelError)
import TeamTavern.Server.Profile.Routes (Identifiers)

type CreateError = Variant
    -- Cookies.
    ( cookieInfoNotPresent :: Map String String
    -- Fields from database.
    , databaseError :: Error
    , unreadableFieldDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , invalidFieldModels ::
        { dtos :: Array LoadFields.FieldDto }
    -- Profile from body.
    , unreadableProfileDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidProfileModel ::
        { dto :: ProfileDto
        , errors :: NonEmptyList ProfileModelError
        }
    -- Profile into database.
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers :: Identifiers
        }
    , unreadableProfileId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    )

logError :: CreateError -> Effect Unit
logError createError = do
    log "Error creating profile"
    createError # match
        { cookieInfoNotPresent: \cookies ->
            logt $ "Couldn't read info from cookies: " <> show cookies
        , databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableFieldDtos: \{ result, errors } -> do
            logt $ "Couldn't read dto from result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading dto resulted in these errors: " <> show errors
        , invalidFieldModels: \{ dtos } -> do
            logt $ "Couldn't create valid fields of dtos from database: "
                <> show dtos
        , unreadableProfileDto: \{ content, errors } -> do
            logt $ "Couldn't read dto out of content: " <> show content
            logt $ "Reading resulted in these errors: " <> show errors
        , invalidProfileModel: \{ dto, errors } -> do
            logt $ "Couldn't validate profile: " <> show dto
            logt $ "Validation resulted in these errors: " <> show errors
        , notAuthorized: \{ cookieInfo, identifiers } -> do
            logt $ "Player with cookie info: " <> show cookieInfo
            logt $ "Not authorized to create profile for identifiers: "
                <> show identifiers
        , unreadableProfileId: \{ result, errors } -> do
            logt $ "Couldn't read profile id from insert result: "
                <> (unsafeStringify $ rows result)
            logt $ "Reading resulted in these errors: " <> show errors
        }
