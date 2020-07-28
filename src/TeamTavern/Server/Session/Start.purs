module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async, examineLeftWithEffect, left)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
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
import TeamTavern.Server.Session.Start.ConfirmEmail (confirmEmail)
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
    model @ { nicknameOrEmail, password, nonce } <- readModel body

    result <- pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        (\client -> do
            -- Check if password hash matches.
            { id, nickname, emailConfirmed } <-
                checkPassword { nicknameOrEmail, password } client

            -- Generate session token.
            token <- Token.generate

            -- Confirm email or ensure it is confirmed.
            case nonce of
                Nothing -> when (not emailConfirmed) $ left
                    $ inj (SProxy :: SProxy "unconfirmedEmail") nicknameOrEmail
                Just nonce' ->
                    confirmEmail { id, nonce: nonce' } client

            -- Create a new session.
            createSession { id, token } client

            pure { id, nickname, token })

    pure result
