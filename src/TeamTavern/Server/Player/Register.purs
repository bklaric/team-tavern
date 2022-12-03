module TeamTavern.Server.Player.Register where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.LogError (logError)
import TeamTavern.Server.Player.Register.ReadDto (readDto)
import TeamTavern.Server.Player.Register.SendResponse (sendResponse)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistration)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)

register :: forall left. Deployment -> Pool -> Cookies -> Body -> Async left Response
register deployment pool cookies body =
    sendResponse deployment $ examineLeftWithEffect logError do
    -- Ensure not signed in.
    ensureNotSignedIn cookies

    -- Read register dto.
    dto <- readDto body

    -- Validate register model.
    { nickname, password } <- validateRegistration dto

    -- Generate password hash.
    hash <- generateHash password

    -- Generate session token.
    token <- Token.generate

    id <- pool # transaction \client -> do
        -- Add player to database.
        id <- addPlayer client { nickname, hash }

        -- Add session to database.
        createSession { id: Id id, token } client

        pure id

    pure { id: Id id, nickname, token }
