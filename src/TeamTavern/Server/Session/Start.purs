module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async, foreach)
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeader)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl, fetchDiscordUser)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (sendConfirmation)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession)
import TeamTavern.Server.Session.Start.CheckDiscord (checkDiscord)
import TeamTavern.Server.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Server.Session.Start.CreateSession (createSession)

start :: ∀ left.
    Deployment -> DiscordApiUrl -> Pool -> Cookies -> StartSession.RequestContent -> Async left _
start deployment discordApiUrl pool cookies body =
    sendResponse "Error starting session" do
    -- Generate session token.
    token <- Token.generate

    {confirmation} <- pool # transaction \client -> do
        {id, confirmation} <- body # match
            { password: \bodyEmail -> do
                -- Check if password hash matches.
                checkPassword bodyEmail client <#> \{id, nickname} ->
                    {id, nickname, confirmation: Nothing}
            , discord: \{accessToken} -> do
                -- Fetch user from Discord API.
                discordUser <- fetchDiscordUser discordApiUrl accessToken
                -- Check if they already have an account, filling in a missing email.
                checkDiscord client discordUser
            }

        -- Replace the session the browser holds, if it holds one.
        revokeSession client cookies
        createSession id token client

        pure {confirmation}

    foreach confirmation $ sendConfirmation deployment

    pure $ noContent $ setCookieHeader deployment token
