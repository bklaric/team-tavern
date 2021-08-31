module TeamTavern.Server.Boarding.Preboard (preboard) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Async as Async
import AsyncV as AsyncV
import Data.Array (elem, fromFoldable)
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
import Record.Extra (pick)
import Simple.JSON (writeJSON)
import TeamTavern.Routes.Preboard (BadContent, RequestContent, OkContent)
import TeamTavern.Routes.Shared.Platform (Platform(..))
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
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (ContactsErrors, validateContactsV)
import TeamTavern.Server.Player.UpdateContacts.WriteContacts (writeContacts)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields) as Player
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile as PlayerProfile
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Profile.Infrastructure.CheckPlayerAlerts (checkPlayerAlerts)
import TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts (checkTeamAlerts)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import TeamTavern.Server.Team.Create.AddTeam (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts as TeamCont
import TeamTavern.Server.Team.Infrastructure.ValidateContacts as TeamLel
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamErrors, validateTeamV)
import TeamTavern.Server.Team.Infrastructure.WriteContacts as TeamIdunno
import Type (type ($))

type PreboardError = Variant
    ( client :: Array String
    , internal :: Array String
    , signedIn :: Array String
    , notAuthenticated :: Array String
    , notAuthorized :: Array String
    , invalidBody :: NonEmptyList $ Variant
        ( team :: TeamErrors
        , playerProfile :: PlayerProfile.ProfileErrors
        , teamProfile :: TeamProfile.ProfileErrors
        , playerContacts :: ContactsErrors
        , teamContacts :: TeamLel.ContactsErrors
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
        ( team :: TeamErrors
        , playerProfile :: PlayerProfile.ProfileErrors
        , teamProfile :: TeamProfile.ProfileErrors
        , playerContacts :: ContactsErrors
        , teamContacts :: TeamLel.ContactsErrors
        , registration :: RegistrationErrors
        )
        -> Effect Unit
    | fields }
invalidBodyHandler = Builder.insert (SProxy :: SProxy "invalidBody") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { team: \errors' -> logt $ "Team errors: " <> show errors'
    , playerProfile: \errors' -> logt $ "Player profile errors: " <> show errors'
    , teamProfile: \errors' -> logt $ "Team profile errors: " <> show errors'
    , playerContacts: \errors' -> logt $ "Player contacts errors: " <> show errors'
    , teamContacts: \errors' -> logt $ "Team contacts errors: " <> show errors'
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
            { team: inj (SProxy :: SProxy "team") <<< Array.fromFoldable
            , playerProfile: inj (SProxy :: SProxy "playerProfile") <<< Array.fromFoldable
            , teamProfile: inj (SProxy :: SProxy "teamProfile") <<< Array.fromFoldable
            , playerContacts: inj (SProxy :: _ "playerContacts") <<< Array.fromFoldable
            , teamContacts: inj (SProxy :: _ "teamContacts") <<< Array.fromFoldable
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
    result <- pool # transaction \client ->
        case content of
        { ilk: 1
        , player: Just player
        , playerProfile: Just profile
        , playerContacts: Just contacts
        , registration
        } -> do
            -- Read fields from database.
            game <- Player.loadFields client content.gameHandle

            -- We only want to patch the selected platform contact.
            let contactsCleaned = contacts
                    { steamId    = if profile.platform == Steam       then contacts.steamId    else Nothing
                    , riotId     = if profile.platform == Riot        then contacts.riotId     else Nothing
                    , battleTag  = if profile.platform == BattleNet   then contacts.battleTag  else Nothing
                    , psnId      = if profile.platform == PlayStation then contacts.psnId      else Nothing
                    , gamerTag   = if profile.platform == Xbox        then contacts.gamerTag   else Nothing
                    , friendCode = if profile.platform == Switch      then contacts.friendCode else Nothing
                    }

            -- Validate data from body.
            { player', profile', contacts', registration' } <-
                { player': _, profile': _, contacts': _, registration': _ }
                <$> validatePlayerV player
                <*> validateProfileV game profile
                <*> validateContactsV [ profile.platform ] contactsCleaned
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

            profileId <- addProfile client id
                { handle: content.gameHandle
                , nickname: unwrap registration'.nickname
                }
                profile'

            writeContacts client id contacts'

            pure
                { teamHandle: Nothing
                , cookieInfo: { id: Id id, nickname: registration'.nickname, token }
                , profileId
                }
        { ilk: 2, team: Just team, teamProfile: Just profile, teamContacts: Just contacts, registration } -> do
            -- Read fields from database.
            game <- Team.loadFields client content.gameHandle

            -- We only want to patch the selected platforms contacts.
            let contactsCleaned = contacts
                    { steamId    = if Steam       `elem` profile.platforms then contacts.steamId    else Nothing
                    , riotId     = if Riot        `elem` profile.platforms then contacts.riotId     else Nothing
                    , battleTag  = if BattleNet   `elem` profile.platforms then contacts.battleTag  else Nothing
                    , psnId      = if PlayStation `elem` profile.platforms then contacts.psnId      else Nothing
                    , gamerTag   = if Xbox        `elem` profile.platforms then contacts.gamerTag   else Nothing
                    , friendCode = if Switch      `elem` profile.platforms then contacts.friendCode else Nothing
                    }

            -- Validate data from body.
            { team', profile', contacts', registration' } <-
                { team': _, profile': _, contacts': _, registration': _ }
                <$> validateTeamV team
                <*> TeamProfile.validateProfileV game profile
                <*> TeamCont.validateContactsV profile.platforms contactsCleaned
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

            profileId <- AddTeamProfile.addProfile client (Id id) handle content.gameHandle profile'

            TeamIdunno.writeContacts client id contacts'

            pure
                { teamHandle: Just handle
                , cookieInfo: { id: Id id, nickname: registration'.nickname, token }
                , profileId
                }
        _ -> Async.left $ inj (SProxy :: SProxy "client") []

    case result.teamHandle of
        Nothing -> checkPlayerAlerts result.profileId pool
        Just _ -> checkTeamAlerts result.profileId pool

    pure $ pick result
