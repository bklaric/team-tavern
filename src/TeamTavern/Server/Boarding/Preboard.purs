module TeamTavern.Server.Boarding.Preboard (preboard) where

import Prelude

import Async (Async)
import Async as Async
import AsyncV as AsyncV
import Data.Array (elem)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (inj, match, over)
import Jarilo (badRequest_, ok)
import Jarilo.Router.Response (AppResponse(..))
import Postgres.Pool (Pool)
import Record.Extra (pick)
import TeamTavern.Routes.Boarding.Preboard as Preboard
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (fetchDiscordUser)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistrationV)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContactsV)
import TeamTavern.Server.Player.UpdateContacts.WriteContacts (writeContacts)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayerV)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields) as Player
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfileV)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile as AddTeamProfile
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields as Team
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile as TeamProfile
import TeamTavern.Server.Profile.Infrastructure.CheckPlayerAlerts (checkPlayerAlerts)
import TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts (checkTeamAlerts)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import TeamTavern.Server.Team.Create (addTeam)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (generateHandle)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts as TeamCont
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (validateTeamV)
import TeamTavern.Server.Team.Infrastructure.WriteContacts as TeamIdunno
import Type.Proxy (Proxy(..))

preboard :: ∀ left. Deployment -> Pool -> Cookies -> Preboard.RequestContent -> Async left _
preboard deployment pool cookies content =
    sendResponse "Error preboarding" do

    -- Ensure the player is not signed in.
    ensureNotSignedIn cookies

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
                    { steamId         = if profile.platform == Steam       then contacts.steamId         else Nothing
                    , riotId          = if profile.platform == Riot        then contacts.riotId          else Nothing
                    , battleTag       = if profile.platform == BattleNet   then contacts.battleTag       else Nothing
                    , eaId            = if profile.platform == Origin      then contacts.eaId            else Nothing
                    , ubisoftUsername = if profile.platform == Ubisoft     then contacts.ubisoftUsername else Nothing
                    , psnId           = if profile.platform == PlayStation then contacts.psnId           else Nothing
                    , gamerTag        = if profile.platform == Xbox        then contacts.gamerTag        else Nothing
                    , friendCode      = if profile.platform == Switch      then contacts.friendCode      else Nothing
                    }

            -- Validate data from body.
            { player', profile', contacts', registration' } <-
                { player': _, profile': _, contacts': _, registration': _ }
                <$> validatePlayerV player
                <*> validateProfileV game profile (Proxy :: _ "playerProfile")
                <*> validateContactsV [ profile.platform ] contactsCleaned (Proxy :: _ "playerContacts")
                <*> validateRegistrationV registration
                # AsyncV.toAsync
                # lmap (map badRequest_)

            {id, nickname} <- registration' # match
                { password: \{email, nickname, password} -> do
                    -- Generate password hash.
                    hash <- generateHash password

                    -- -- Add player.
                    id <- addPlayer client { email, nickname, hash}
                        # lmap (map (over { badRequest: \(AppResponse headers body) -> AppResponse headers (Nea.singleton body) }))
                    -- id <- pure 10

                    pure {id, nickname}
                , discord: \{nickname, accessToken} -> do
                    discordUser <- fetchDiscordUser accessToken
                    id <- addPlayerDiscord client nickname discordUser
                        # lmap (map (over { badRequest: \(AppResponse headers body) -> AppResponse headers (Nea.singleton body) }))
                    pure {id, nickname}
                }

            -- Generate session token.
            token <- Token.generate

            -- Create a new session.
            createSession id token client

            updateDetails client id player'

            profileId <- addProfile client id
                { handle: content.gameHandle
                , nickname: unwrap nickname
                }
                profile'

            writeContacts client id contacts'

            pure
                { teamHandle: Nothing
                , cookieInfo: { id: Id id, nickname, token }
                , profileId
                }
        { ilk: 2
        , team: Just team
        , teamProfile: Just profile
        , teamContacts: Just contacts
        , registration
        } -> do
            -- Read fields from database.
            game <- Team.loadFields client content.gameHandle

            -- We only want to patch the selected platforms contacts.
            let contactsCleaned = contacts
                    { steamId         = if Steam       `elem` profile.platforms then contacts.steamId         else Nothing
                    , riotId          = if Riot        `elem` profile.platforms then contacts.riotId          else Nothing
                    , battleTag       = if BattleNet   `elem` profile.platforms then contacts.battleTag       else Nothing
                    , eaId            = if Origin      `elem` profile.platforms then contacts.eaId            else Nothing
                    , ubisoftUsername = if Ubisoft     `elem` profile.platforms then contacts.ubisoftUsername else Nothing
                    , psnId           = if PlayStation `elem` profile.platforms then contacts.psnId           else Nothing
                    , gamerTag        = if Xbox        `elem` profile.platforms then contacts.gamerTag        else Nothing
                    , friendCode      = if Switch      `elem` profile.platforms then contacts.friendCode      else Nothing
                    }

            -- Validate data from body.
            { team', profile', contacts', registration' } <-
                { team': _, profile': _, contacts': _, registration': _ }
                <$> validateTeamV team
                <*> TeamProfile.validateProfileV game profile (Proxy :: _ "teamProfile")
                <*> TeamCont.validateContactsV profile.platforms contactsCleaned (Proxy :: _ "teamContacts")
                <*> validateRegistrationV registration
                # AsyncV.toAsync
                # lmap (map badRequest_)

            {id, nickname} <- registration' # match
                { password: \{email, nickname, password} -> do
                    -- Generate password hash.
                    hash <- generateHash password

                    -- Add player.
                    id <- addPlayer client { email, nickname, hash}
                        # lmap (map (over { badRequest: \(AppResponse headers body) -> AppResponse headers (Nea.singleton body) }))

                    pure {id, nickname}
                , discord: \{nickname, accessToken} -> do
                    discordUser <- fetchDiscordUser accessToken
                    id <- addPlayerDiscord client nickname discordUser
                        # lmap (map (over { badRequest: \(AppResponse headers body) -> AppResponse headers (Nea.singleton body) }))
                    pure {id, nickname}
                }

            -- Generate session token.
            token <- Token.generate

            -- Create a new session.
            createSession id token client

            let generatedHandle = generateHandle team'.organization nickname

            { id: teamId, handle } <- addTeam client (Id id) generatedHandle team'

            profileId <- AddTeamProfile.addProfile client (Id id) handle content.gameHandle profile'

            TeamIdunno.writeContacts client teamId contacts'

            pure
                { teamHandle: Just handle
                , cookieInfo: { id: Id id, nickname, token }
                , profileId
                }
        _ -> Async.left $ Terror (badRequest_ $ Nea.singleton $ inj (Proxy :: _ "other") {}) []

    case result.teamHandle of
        Nothing -> checkPlayerAlerts result.profileId pool
        Just _ -> checkTeamAlerts result.profileId pool

    pure $ ok (setCookieHeaderFull deployment result.cookieInfo) (pick result :: Preboard.OkContent)
