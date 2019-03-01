module TeamTavern.Session.Start where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Session.Start.CheckPassword (checkPassword)
import TeamTavern.Session.Start.ConfirmEmail (confirmEmail)
import TeamTavern.Session.Start.CreateSession (createSession)
import TeamTavern.Session.Start.GenerateToken (generateToken)
import TeamTavern.Session.Start.LogError (logError)
import TeamTavern.Session.Start.ReadModel (readModel)
import TeamTavern.Session.Start.SendResponse (sendResponse)

start :: forall left. Pool -> Map String String -> Body -> Async left Response
start pool cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure player isn't signed in.
    ensureNotSignedIn cookies

    -- Read start model.
    model @ { nicknameOrEmail, password, nonce } <- readModel body

    result <- pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        (\client -> do
            -- Check if password hash matches.
            playerId <- checkPassword { nicknameOrEmail, password } client

            -- Generate session token.
            token <- generateToken

            -- Create a new session.
            case nonce of
                Just nonce' ->
                    confirmEmail { playerId, nonce: nonce' } client
                Nothing -> pure unit
            createSession { playerId, token } client

            pure { playerId, token })

    pure result
