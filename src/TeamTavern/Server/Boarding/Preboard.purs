module TeamTavern.Server.Boarding.Preboard (preboard) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async as Async
import AsyncV as AsyncV
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label)
import Data.Either (isRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (inj, match, onMatch)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, internalServerError__, ok_)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import Simple.JSON (writeJSON)
import TeamTavern.Routes.Preboard (BadContent, RequestContent, OkContent)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Domain.Nonce (generateNonce)
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.SendEmail (sendEmail)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistrationV)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Profile.Infrastructure.ConvertFields (convertFields)
import TeamTavern.Server.Team.Create.AddTeam (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (validateTeamV)

-- errorResponse :: RegisterError -> Response
errorResponse = onMatch
    { invalidBody: \errors ->
        errors
        # fromFoldable
        <#> match
            { player: inj (SProxy :: SProxy "player") <<< Array.fromFoldable
            , team: inj (SProxy :: SProxy "team") <<< Array.fromFoldable
            , playerProfile: inj (SProxy :: SProxy "playerProfile") <<< Array.fromFoldable
            , teamProfile: inj (SProxy :: SProxy "teamProfile") <<< Array.fromFoldable
            , registration: inj (SProxy :: SProxy "registration") <<< Array.fromFoldable
            }
        # (writeJSON :: BadContent -> String)
        # badRequest_
    }
    (const internalServerError__)

-- successResponse :: SendResponseModel -> Response
successResponse = ok_ <<< (writeJSON :: OkContent -> String) -- const noContent_

-- sendResponse ::
--     Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

preboard :: Pool -> Maybe Client -> Cookies -> Body -> Async _ Response
preboard pool emailClient cookies body =
    -- sendResponse $ examineLeftWithEffect logError do
    sendResponse $ examineLeftWithEffect (unsafeStringify >>> log) do

    -- Ensure the player is not signed in.
    ensureNotSignedIn cookies

    -- Read data from body.
    (content :: RequestContent) <- readJsonBody body

    -- Start the transaction.
    { teamHandle, registration, nonce } <- pool # transaction \client -> do
        -- Read fields from database.
        fields <- loadFields client content.gameHandle

        case content of
            { ilk: 1, player: Just player, playerProfile: Just profile, registration } -> do
                -- Validate data from body.
                { player', profile', registration' } <-
                    { player': _, profile': _, registration': _ }
                    <$> validatePlayerV player
                    <*> validateProfileV fields profile
                    <*> validateRegistrationV registration
                    # AsyncV.toAsync
                    # label (SProxy :: SProxy "invalidBody")

                -- Generate password hash.
                hash <- generateHash registration'.password

                -- Generate email confirmation nonce.
                nonce <- generateNonce

                -- Add player.
                playerId <- addPlayer client
                    { email: registration'.email
                    , nickname: registration'.nickname
                    , hash
                    , nonce
                    }

                updateDetails client playerId player'

                addProfile client playerId
                    { handle: content.gameHandle
                    , nickname: unwrap registration'.nickname
                    }
                    profile'

                pure { teamHandle: Nothing, registration: registration', nonce }
            { ilk: 2, team: Just team, teamProfile: Just profile, registration } -> do
                -- Validate data from body.
                { team', profile', registration' } <-
                    { team': _, profile': _, registration': _ }
                    <$> validateTeamV team
                    <*> TeamProfile.validateProfileV (convertFields fields) profile
                    <*> validateRegistrationV registration
                    # AsyncV.toAsync
                    # label (SProxy :: SProxy "invalidBody")

                -- Generate password hash.
                hash <- generateHash registration'.password

                -- Generate email confirmation nonce.
                nonce <- generateNonce

                -- Add player.
                playerId <- addPlayer client
                    { email: registration'.email
                    , nickname: registration'.nickname
                    , hash
                    , nonce
                    }

                { handle } <- addTeam client (Id playerId) (generateHandle team'.name) team'

                AddTeamProfile.addProfile client (Id playerId) handle content.gameHandle profile'

                pure $ { teamHandle: Just handle, registration: registration', nonce }
            _ -> Async.left $ inj (SProxy :: SProxy "client") []

    -- Send confirmation email.
    emailSent <-
        sendEmail emailClient
        { email: registration.email
        , nickname: registration.nickname
        , nonce
        , preboarded: true
        }
        # Async.examineLeftWithEffect (log <<< unsafeStringify)
        # Async.attempt
        <#> isRight

    pure
        { email: unwrap registration.email
        , nickname: unwrap registration.nickname
        , emailSent
        , teamHandle
        }
