module TeamTavern.Server.Wizard.Onboard where

import Prelude

import Async (Async(..), alwaysRight, examineLeftWithEffect)
import Async as Async
import Async.Validated (fromValidated) as Async
import AsyncV (AsyncV(..))
import AsyncV as AsyncV
import Data.Array (catMaybes, fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Either (Either(..), either, hush)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant, inj, match, onMatch)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, ok_)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import Postmark.Async.Client as Postmark
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import Simple.JSON (writeJSON)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Player.Domain.About as About
import TeamTavern.Server.Player.Domain.Email as Email
import TeamTavern.Server.Player.Domain.Hash as Hash
import TeamTavern.Server.Player.Domain.Nickname as Nickname
import TeamTavern.Server.Player.Domain.Nonce as Nonce
import TeamTavern.Server.Player.Domain.Password as Password
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.SendEmail (SendEmailModel, SendEmailError)
import TeamTavern.Server.Player.Register.ValidateModel (RegisterModel, ValidateModelError, RegisterModelError)
import TeamTavern.Server.Player.UpdateDetails.ReadUpdate (UpdateDetailsModel)
import TeamTavern.Server.Player.UpdateDetails.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdateDetails.ValidateBirthday (validateOptionalBirthday)
import TeamTavern.Server.Player.UpdateDetails.ValidateLocation (validateLocation)
import TeamTavern.Server.Player.UpdateDetails.ValidateDiscordTag (validateOptionalDiscordTag)
import TeamTavern.Server.Player.UpdateDetails.ValidateLangugase (validateLanguages)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (validateTimespan)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimezone (validateTimezone)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as LoadFields
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile as ReadProfile
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (Field(..))
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues as ValidateFieldValues
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile, ProfileError)
import TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions as ValidateAmbitions
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary as ValidateSummary

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

validatePlayerDetails
    :: forall errors
    .  PlayerDetails
    -> AsyncV
        (NonEmptyList (Variant
            ( invalidDiscordTag :: { message :: Array String }
            , invalidAbout :: { message :: Array String }
            | errors ))
        )
        UpdateDetailsModel
validatePlayerDetails details = do
    birthday <- AsyncV.fromEffect $ validateOptionalBirthday details.birthday
    let location = validateLocation details.location
        languages = validateLanguages details.languages
        microphone = details.microphone
    discordTag <- validateOptionalDiscordTag details.discordTag
        # AsyncV.fromValidated
        # AsyncV.labelMap (SProxy :: SProxy "invalidDiscordTag") \error ->
            { message:
                [ "Discord tag could not be validated."
                , "Discord tag:", show details.discordTag
                , "Error: ", show error
                ]
            }
    let timezone = validateTimezone details.timezone
        onlineWeekday = timezone >>= (const $ validateTimespan details.weekdayFrom details.weekdayTo)
        onlineWeekend = timezone >>= (const $ validateTimespan details.weekendFrom details.weekendTo)
    about <- About.create details.about
        # AsyncV.fromValidated
        # AsyncV.labelMap (SProxy :: SProxy "invalidAbout") \error ->
            { message: [ "About is too long or something: ", show error ] }
    pure { birthday, location
         , languages, microphone, discordTag
         , timezone, onlineWeekday, onlineWeekend
         , about
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
    ( invalidDiscordTag :: {}
    , invalidAbout :: {}
    , ambitions :: Array String
    , url :: { key :: String, message :: Array String }
    , missing :: { key :: String, message :: Array String }
    ))

-- errorResponse :: RegisterError -> Response
errorResponse = onMatch
    { invalidBody: \errors ->
            errors
            <#> match
                { invalidDiscordTag: const [ inj (SProxy :: SProxy "invalidDiscordTag") {} ]
                , invalidAbout: const [ inj (SProxy :: SProxy "invalidAbout") {} ]
                , profile: \{ errors } -> Array.fromFoldable errors

                    -- \profileErrors ->
                    -- (profileErrors.errors :: NonEmptyList ProfileError)
                    -- <#> (match
                    --     { summary: const $ Array.singleton $ inj (SProxy :: SProxy "summary") {}
                    --     , fieldValues: \fieldValueErrors ->
                    --         fieldValueErrors
                    --         <#> onMatch
                    --             { invalidUrlFieldValue: \{ fieldValue: { fieldKey }, errors: errors' } ->
                    --                 Just $ inj (SProxy :: SProxy "invalidUrl") { fieldKey }
                    --             , missingFieldValue: \{ field: (Field _ key _ _) } ->
                    --                 Just $ inj (SProxy :: SProxy "missing") { fieldKey: key }
                    --             }
                    --             (const Nothing)
                    --         # fromFoldable
                    --         # catMaybes
                    --     })
                    -- # fromFoldable
                    -- # join
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
            <$> validatePlayerDetails body'.personalDetails
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
