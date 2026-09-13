module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async)
import Data.Newtype (wrap)
import Data.Variant (match)
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl, fetchDiscordUser)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CheckDiscord (checkDiscord)
import TeamTavern.Server.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Server.Session.Start.CreateSession (createSession)

start :: ∀ left.
    Deployment -> DiscordApiUrl -> Pool -> Cookies -> StartSession.RequestContent -> Async left _
start deployment discordApiUrl pool cookies body =
    sendResponse "Error starting session" do
    -- Ensure player isn't signed in.
    ensureNotSignedIn cookies

    -- Generate session token.
    token <- Token.generate

    pool # transaction \client -> do
        {id, nickname} <- body # match
            { password: \bodyEmail -> do
                -- Check if password hash matches.
                checkPassword bodyEmail client
            , discord: \{accessToken} -> do
                -- Fetch user from Discord API.
                discordUser <- fetchDiscordUser discordApiUrl accessToken
                -- Check if they already have an account, filling in a missing contact email.
                checkDiscord client discordUser
            }

        -- Create a new session.
        createSession id token client

        pure $ noContent $ setCookieHeaderFull deployment
            {id: wrap id, nickname: wrap nickname, token}
