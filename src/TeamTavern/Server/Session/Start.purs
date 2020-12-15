module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Symbol (SProxy(..))
import Data.Variant (inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Architecture.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import TeamTavern.Server.Session.Start.LogError (logError)
import TeamTavern.Server.Session.Start.ReadModel (readModel)
import TeamTavern.Server.Session.Start.SendResponse (sendResponse)

start :: forall left. Deployment -> Pool -> Map String String -> Body -> Async left Response
start deployment pool cookies body =
    sendResponse deployment $ examineLeftWithEffect logError do
    -- Ensure player isn't signed in.
    ensureNotSignedIn cookies

    -- Read start model.
    model <- readModel body

    pool # withTransaction (inj (SProxy :: SProxy "databaseError")) \client -> do
        -- Check if password hash matches.
        { id, nickname } <- checkPassword model client

        -- Generate session token.
        token <- Token.generate

        -- Create a new session.
        createSession { id, token } client

        pure { id, nickname, token }
