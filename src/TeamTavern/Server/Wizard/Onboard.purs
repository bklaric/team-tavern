module TeamTavern.Server.Wizard.Onboard where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match, onMatch)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, internalServerError__, noContent_)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import Simple.JSON (writeJSON)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (PlayerError, validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions as ValidateAmbitions

type PlayerDetails =
    { birthday :: Maybe String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , discordTag :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

type ProfileDetails =
    { fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , newOrReturning :: Boolean
    , ambitions :: String
    }

type RequestBody =
    { handle :: String
    , personalDetails :: PlayerDetails
    , profileDetails :: ProfileDetails
    }

readRequestBody :: forall errors.
    Body -> Async (Variant (unreadableBody :: { message :: Array String } | errors)) RequestBody
readRequestBody body = do
    content <- readBody body
    readJSON content # labelMap (SProxy :: SProxy "unreadableBody") \errors ->
        { message:
            [ "Body could not be read."
            , "Content:" , content
            , "Errors:", show errors
            ]
        }

validateProfileDetails
    :: forall errors
    .  Array LoadFields.Field -> ReadProfile.Profile
    -> AsyncV (NonEmptyList (Variant (profile :: _ | errors))) Profile
validateProfileDetails fields details = do
    { fieldValues: _, newOrReturning: details.newOrReturning, ambitions: _ }
    <$> ValidateFieldValues.validateFieldValues fields details.fieldValues
    <*> ValidateAmbitions.validateAmbitions details.ambitions
    # AsyncV.fromValidated
    # AsyncV.labelMap (SProxy :: SProxy "profile") { profile: details, errors: _ }

type BadRequestContent = Array (Variant
    ( ambitions :: Array String
    , url :: { key :: String, message :: Array String }
    , missing :: { key :: String, message :: Array String }

    , player :: Array PlayerError
    ))

-- errorResponse :: RegisterError -> Response
errorResponse = onMatch
    { invalidBody: \errors ->
            errors
            <#> match
                { profile: \{ errors } -> Array.fromFoldable errors
                , player: \errors -> [ inj (SProxy :: SProxy "player") (Array.fromFoldable errors) ]
                }
            # fromFoldable
            # join
            # (writeJSON :: BadRequestContent -> String)
            # badRequest_
    }
    (const internalServerError__)

-- successResponse :: SendResponseModel -> Response
successResponse = const noContent_

-- sendResponse ::
--     Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

onboard :: Pool -> Cookies -> Body -> Async _ Response
onboard pool cookies body =
    -- sendResponse $ examineLeftWithEffect logError do
    sendResponse $ examineLeftWithEffect (unsafeStringify >>> log) do

    -- Ensure the player is not signed in.
    cookieInfo <- ensureSignedIn pool cookies

    -- Read data from body.
    body' <- readRequestBody body

    -- Start the transaction.
    pool # withTransaction (inj (SProxy :: SProxy "databaseError")) \client -> do
        -- Read fields from database.
        fields <- loadFields client body'.handle

        -- Validate data from body.
        (validatedBody :: _) <-
            ({ player: _, profile: _ }
            <$> validatePlayerV body'.personalDetails
            <*> validateProfileDetails fields body'.profileDetails)
            # AsyncV.toAsync
            # label (SProxy :: SProxy "invalidBody")

        -- Update player details.
        updateDetails client (unwrap cookieInfo.id) validatedBody.player

        -- Add profile.
        addProfile client (unwrap cookieInfo.id)
            { handle: body'.handle
            , nickname: unwrap cookieInfo.nickname
            }
            validatedBody.profile
