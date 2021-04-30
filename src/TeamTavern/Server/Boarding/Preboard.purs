module TeamTavern.Server.Boarding.Preboard (preboard) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async as Async
import AsyncV as AsyncV
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Effect (Effect, foreachE)
import Global.Unsafe (unsafeStringify)
import Node.Errors (Error)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok, unauthorized__)
import Postgres.Error as Postgres
import Postgres.Pool (Pool)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (writeJSON)
import TeamTavern.Routes.Preboard (BadContent, RequestContent, OkContent)
import TeamTavern.Server.Architecture.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn')
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logt, notAuthenticatedHandler, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.ValidateRegistration (RegistrationErrors, validateRegistrationV)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (PlayerErrors, validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields as Player
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile as PlayerProfile
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import TeamTavern.Server.Team.Create.AddTeam (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamErrors, validateTeamV)
import Type (type ($))

type PreboardError = Variant
    ( client :: Array String
    , internal :: Array String
    , signedIn :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , invalidBody :: NonEmptyList $ Variant
        ( player :: PlayerErrors
        , team :: TeamErrors
        , playerProfile :: PlayerProfile.ProfileErrors
        , teamProfile :: TeamProfile.ProfileErrors
        , registration :: RegistrationErrors
        )
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Postgres.Error
        }
    , randomError :: Error
    )

invalidBodyHandler :: forall fields. Lacks "invalidBody" fields =>
    Builder (Record fields)
    { invalidBody ::
        NonEmptyList $ Variant
        ( player :: PlayerErrors
        , team :: TeamErrors
        , playerProfile :: PlayerProfile.ProfileErrors
        , teamProfile :: TeamProfile.ProfileErrors
        , registration :: RegistrationErrors
        )
        -> Effect Unit
    | fields }
invalidBodyHandler = Builder.insert (SProxy :: SProxy "invalidBody") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { player: \errors' -> logt $ "Player errors: " <> show errors'
    , team: \errors' -> logt $ "Team errors: " <> show errors'
    , playerProfile: \errors' -> logt $ "Player profile errors: " <> show errors'
    , teamProfile: \errors' -> logt $ "Team profile errors: " <> show errors'
    , registration: \errors' -> logt $ "Registration errors: " <> show errors'
    }

logError :: PreboardError -> Effect Unit
logError = Log.logError "Error preboarding"
    ( internalHandler
    >>> clientHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    >>> invalidBodyHandler
    >>> (Builder.insert (SProxy :: SProxy "nicknameTaken") (unsafeStringify >>> logt))
    >>> (Builder.insert (SProxy :: SProxy "signedIn") (unsafeStringify >>> logt))
    >>> (Builder.insert (SProxy :: SProxy "randomError") (unsafeStringify >>> logt))
    )

type PreboardResult =
    { teamHandle :: Maybe String
    , cookieInfo :: CookieInfo
    }

errorResponse :: PreboardError -> Response
errorResponse = match
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
    , nicknameTaken: \error ->
        [ inj (SProxy :: SProxy "nicknameTaken") [ unsafeStringify error ] ]
        # (writeJSON :: BadContent -> String)
        # badRequest_
    , client: const badRequest__
    , internal: const internalServerError__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    , signedIn: const forbidden__
    , randomError: const internalServerError__
    }

successResponse :: Deployment -> PreboardResult -> Response
successResponse deployment { teamHandle, cookieInfo } =
    ok (setCookieHeaderFull deployment cookieInfo)
    ((writeJSON :: OkContent -> String) { teamHandle })

sendResponse ::
    Deployment -> Async PreboardError PreboardResult -> (forall left. Async left Response)
sendResponse deployment = alwaysRight errorResponse $ successResponse deployment

preboard :: forall left. Deployment -> Pool -> Cookies -> Body -> Async left Response
preboard deployment pool cookies body =
    sendResponse deployment $ examineLeftWithEffect logError do

    -- Ensure the player is not signed in.
    ensureNotSignedIn' cookies

    -- Read data from body.
    (content :: RequestContent) <- readJsonBody body

    -- Start the transaction.
    pool # transaction \client ->
        case content of
        { ilk: 1, player: Just player, playerProfile: Just profile, registration } -> do
            -- Read fields from database.
            game <- Player.loadFields client content.gameHandle

            -- Validate data from body.
            { player', profile', registration' } <-
                { player': _, profile': _, registration': _ }
                <$> validatePlayerV player
                <*> validateProfileV game profile
                <*> validateRegistrationV registration
                # AsyncV.toAsync
                # label (SProxy :: SProxy "invalidBody")

            -- Generate password hash.
            hash <- generateHash registration'.password

            -- Generate session token.
            token <- Token.generate

            -- Add player.
            id <- addPlayer client
                { nickname: registration'.nickname
                , hash
                }

            -- Create a new session.
            createSession { id: Id id, token } client

            updateDetails client id player'

            addProfile client id
                { handle: content.gameHandle
                , nickname: unwrap registration'.nickname
                }
                profile'

            pure
                { teamHandle: Nothing
                , cookieInfo: { id: Id id, nickname: registration'.nickname, token }
                }
        { ilk: 2, team: Just team, teamProfile: Just profile, registration } -> do
            -- Read fields from database.
            game <- Team.loadFields client content.gameHandle

            -- Validate data from body.
            { team', profile', registration' } <-
                { team': _, profile': _, registration': _ }
                <$> validateTeamV team
                <*> TeamProfile.validateProfileV game profile
                <*> validateRegistrationV registration
                # AsyncV.toAsync
                # label (SProxy :: SProxy "invalidBody")

            -- Generate password hash.
            hash <- generateHash registration'.password

            -- Generate session token.
            token <- Token.generate

            -- Add player.
            id <- addPlayer client
                { nickname: registration'.nickname
                , hash
                }

            -- Create a new session.
            createSession { id: Id id, token } client

            let generatedHandle = generateHandle team'.organization registration'.nickname

            { handle } <- addTeam client (Id id) generatedHandle team'

            AddTeamProfile.addProfile client (Id id) handle content.gameHandle profile'

            pure
                { teamHandle: Just handle
                , cookieInfo: { id: Id id, nickname: registration'.nickname, token }
                }
        _ -> Async.left $ inj (SProxy :: SProxy "client") []
