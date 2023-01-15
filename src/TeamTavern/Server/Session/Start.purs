module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async)
import Data.Newtype (wrap)
import Jarilo (noContent)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Server.Session.Start.CreateSession (createSession)

start :: ∀ left.
    Deployment -> Pool -> Cookies -> StartSession.RequestContent -> Async left _
start deployment pool cookies body =
    sendResponse "Error starting session" do
    -- Ensure player isn't signed in.
    ensureNotSignedIn cookies

    pool # transaction \client -> do
        -- Check if password hash matches.
        { id, nickname } <- checkPassword body client

        -- Generate session token.
        token <- Token.generate

        -- Create a new session.
        createSession id token client

        pure $ noContent $ setCookieHeaderFull deployment
            { id: wrap id, nickname: wrap nickname, token }
