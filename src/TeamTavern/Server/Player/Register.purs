module TeamTavern.Server.Player.Register where

import Prelude

import Async (Async)
import Data.Variant (match)
import Jarilo (noContent)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (fetchDiscordUser)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistration)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)

register :: ∀ left. Deployment -> Pool -> Cookies -> RegisterPlayer.RequestContent -> Async left _
register deployment pool cookies content =
    sendResponse "Error registering player" do
    -- Ensure not signed in.
    ensureNotSignedIn cookies

    -- Validate register model.
    registration <- validateRegistration content

    -- Generate session token.
    token <- Token.generate

    {id, nickname} <- registration # match
        { password: \{email, nickname, password} -> do
            -- Generate password hash.
            hash <- generateHash password

            pool # transaction \client -> do
                -- Add player to database.
                id <- addPlayer client { email, nickname, hash }

                -- Add session to database.
                createSession id token client

                pure {id, nickname}

        , discord: \{nickname, accessToken} -> do
            discordUser <- fetchDiscordUser accessToken
            pool # transaction \client -> do
                id <- addPlayerDiscord client nickname discordUser
                createSession id token client
                pure {id, nickname}
        }

    pure $ noContent $ setCookieHeaderFull deployment
        {id: Id id, nickname, token}
