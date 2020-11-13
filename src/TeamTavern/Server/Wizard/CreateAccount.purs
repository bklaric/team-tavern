module TeamTavern.Server.Wizard.CreateAccount where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async (attempt) as Async
import AsyncV (AsyncV)
import AsyncV as AsyncV
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Either (Either(..), either, hush)
import Data.List.Types (NonEmptyList)
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
import Perun.Response (Response, badRequest_, internalServerError__, ok_)
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
import TeamTavern.Server.Player.Domain.Email as Email
import TeamTavern.Server.Player.Domain.Hash as Hash
import TeamTavern.Server.Player.Domain.Nickname as Nickname
import TeamTavern.Server.Player.Domain.Nonce as Nonce
import TeamTavern.Server.Player.Domain.Password as Password
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.SendEmail (SendEmailModel)
import TeamTavern.Server.Player.Register.ValidateModel (RegisterModel, RegisterModelError)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (PlayerError, validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)

type PersonalDetails =
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

type RegistrationDetails =
    { email :: String
    , nickname :: String
    , password :: String
    }

type RequestBody =
    { handle :: String
    , personalDetails :: PersonalDetails
    , profileDetails :: ProfileDetails
    , registrationDetails :: RegistrationDetails
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

validateRegistrationDetails :: forall errors.
    RegistrationDetails ->
    AsyncV (NonEmptyList (Variant
        ( invalidRegistration ::
            { details :: RegistrationDetails
            , errors :: NonEmptyList RegisterModelError }
        | errors )
        )) RegisterModel
validateRegistrationDetails details = do
    { email: _, nickname: _, password: _ }
        <$> (Email.create details.email
            # Validated.label (SProxy :: SProxy "email"))
        <*> (Nickname.create details.nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (Password.create details.password
            # Validated.label (SProxy :: SProxy "password"))
        # AsyncV.fromValidated
        # AsyncV.labelMap (SProxy :: SProxy "invalidRegistration")
            { details, errors: _ }

message :: SendEmailModel -> Message
message { email, nickname, nonce } =
    { to: unwrap email
    , from: "TeamTavern admin@teamtavern.net"
    , subject: toNullable $ Just "TeamTavern registration"
    , htmlBody: toNullable $ Just $
        "Hi " <> unwrap nickname <> ",<br /><br />"
        <> "Thank you for registering to TeamTavern. Your profile has been successfully created! "
        <> "Please open the link below to sign in and verify your email address:<br /><br />"
        <> "<a href=\"https://www.teamtavern.net/signin?nonce=" <> unwrap nonce <> "\">https://www.teamtavern.net/signin?nonce=" <> unwrap nonce <> "</a><br /><br />"
        <> "Your profile will be visible to others once you confirm your email address. "
        <> "Should you have any questions or feedback, please contact <a href=\"mailto:admin@teamtavern.net\">admin@teamtavern.net</a>. Thank you for your time.<br /><br />"
        <> "Happy playing!"
    , textBody: toNullable Nothing
    }

sendEmail :: forall left. Maybe Client -> SendEmailModel -> Async left (Either Error Unit)
sendEmail client model @ { email, nickname } = let
    message' = message model
    in
    case client of
        Nothing -> do
            logShow message'
            pure $ Right unit
        Just client' ->
            client'
            # Postmark.sendEmail message'
            # Async.attempt

type OkContent =
    { email :: String
    , nickname :: String
    , emailSent :: Boolean
    }

type IdentifiersErrorContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    , invalidPassword :: {}
    )

type BadRequestContent = Variant
    ( invalidBody :: Array (Variant
        ( invalidProfile :: Array
            (Variant
                ( ambitions :: Array String
                , url :: { key :: String, message :: Array String }
                , missing :: { key :: String, message :: Array String }
                )
            )
        , invalidRegistration :: Array (Variant
            ( invalidEmail :: {}
            , invalidNickname :: {}
            , invalidPassword :: {}
            ))

        , player :: Array PlayerError
        ))
    , emailTaken :: {}
    , nicknameTaken :: {}
    )

-- errorResponse :: RegisterError -> Response
errorResponse = onMatch
    { invalidBody: \errors ->
            errors
            <#> match
                { player: \errors -> inj (SProxy :: SProxy "player") (Array.fromFoldable errors)
                , profile: \errors -> inj (SProxy :: SProxy "invalidProfile") $ Array.fromFoldable errors
                , invalidRegistration: \{ errors } ->
                    errors
                    <#> (match
                        { email: const $ inj (SProxy :: SProxy "invalidEmail") {}
                        , nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
                        , password: const $ inj (SProxy :: SProxy "invalidPassword") {}
                        })
                    # fromFoldable
                    # inj (SProxy :: SProxy "invalidRegistration")
                }
            # fromFoldable
            # inj (SProxy :: SProxy "invalidBody")
            # (writeJSON :: BadRequestContent -> String)
            # badRequest_
    , emailTaken: (const $ inj (SProxy :: SProxy "emailTaken") {})
            >>> (writeJSON :: BadRequestContent -> String)
            >>> badRequest_
    , nicknameTaken: (const $ inj (SProxy :: SProxy "nicknameTaken") {})
            >>> (writeJSON :: BadRequestContent -> String)
            >>> badRequest_
    }
    (const internalServerError__)

-- successResponse :: SendResponseModel -> Response
successResponse { email, nickname, result } =
    ok_ $ writeJSON
    ({ email: email
    , nickname: nickname
    , emailSent: isNothing result
    } :: OkContent)

-- sendResponse ::
--     Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

createAccount ::
  Pool
  -> Maybe Client
     -> Cookies
        -> Body
           -> Async
                _
                Response
createAccount pool emailClient cookies body =
    -- sendResponse $ examineLeftWithEffect logError do
    sendResponse $ examineLeftWithEffect (unsafeStringify >>> log) do

    -- Ensure the player is not signed in.
    ensureNotSignedIn cookies

    -- Read data from body.
    body' <- readRequestBody body

    -- Start the transaction.
    { validatedBody, nonce } <- pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Read fields from database.
            fields <- loadFields client body'.handle

            -- Validate data from body.
            (validatedBody :: _) <-
                ({ personal: _, profile: _, registration: _ }
                <$> validatePlayerV body'.personalDetails
                <*> validateProfileV fields body'.profileDetails
                <*> validateRegistrationDetails body'.registrationDetails)
                # AsyncV.toAsync
                # label (SProxy :: SProxy "invalidBody")

            -- Generate password hash.
            hash <- Hash.generate validatedBody.registration.password

            -- Generate email confirmation nonce.
            nonce <- Nonce.generate

            -- Add player.
            playerId <- addPlayer client
                { email: validatedBody.registration.email
                , nickname: validatedBody.registration.nickname
                , hash
                , nonce
                }

            updateDetails client playerId validatedBody.personal

            -- Add profile.
            addProfile client playerId
                { handle: body'.handle
                , nickname: validatedBody.registration.nickname # unwrap
                }
                validatedBody.profile

            pure { validatedBody, nonce }

    -- Send confirmation email.
    result <- sendEmail emailClient
        { email: validatedBody.registration.email
        , nickname: validatedBody.registration.nickname
        , nonce
        }

    pure
        { email: validatedBody.registration.email # unwrap
        , nickname: validatedBody.registration.nickname # unwrap
        , result: either Right Left result # hush
        }
