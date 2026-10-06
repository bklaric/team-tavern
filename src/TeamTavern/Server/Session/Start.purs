module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async, foreach)
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeader)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Environment (Environment)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl, fetchDiscordUser)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ResolveSteamId (SteamApi)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.SteamOpenId (SteamOpenIdUrl, verifySteamReturn)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (sendConfirmation)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession)
import TeamTavern.Server.Session.Start.CheckDiscord (checkDiscord)
import TeamTavern.Server.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Server.Session.Start.CheckSteam (checkSteam)
import TeamTavern.Server.Session.Start.CreateSession (createSession)

start :: ∀ left.
    Environment -> Mailer -> DiscordApiUrl -> SteamOpenIdUrl -> SteamApi -> Pool -> Cookies -> String
    -> StartSession.RequestContent -> Async left _
start environment mailer discordApiUrl steamOpenIdUrl steamApi pool cookies origin body =
    sendResponse "Error starting session" do
    -- Generate session token.
    token <- Token.generate

    -- Replace the session the browser holds, if it holds one.
    let startFor client id confirmation = do
            revokeSession client cookies
            createSession id token client
            pure {confirmation}

    -- Discord and Steam are asked before the transaction, which holds a
    -- connection from the pool until it ends.
    {confirmation} <- body # match
        { password: \bodyEmail -> pool # transaction \client -> do
            {id} <- checkPassword bodyEmail client
            startFor client id Nothing
        , discord: \{accessToken} -> do
            discordUser <- fetchDiscordUser discordApiUrl accessToken
            pool # transaction \client -> do
                -- Check if they already have an account, filling in a missing email.
                {id, confirmation} <- checkDiscord client discordUser
                startFor client id confirmation
        , steam: \{assertion} -> do
            steamId <- verifySteamReturn steamOpenIdUrl origin pool assertion
            {id} <- checkSteam steamApi pool steamId
            pool # transaction \client -> startFor client id Nothing
        }

    foreach confirmation $ sendConfirmation mailer

    pure $ noContent $ setCookieHeader environment token
