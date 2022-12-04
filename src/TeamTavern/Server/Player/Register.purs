module TeamTavern.Server.Player.Register where

import Prelude

import Async (Async)
import Jarilo (noContent)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistration)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)

register :: forall left. Deployment -> Pool -> Cookies -> RegisterPlayer.RequestContent -> Async left _
register deployment pool cookies content =
    sendResponse "Error registering player" do
    -- Ensure not signed in.
    ensureNotSignedIn cookies

    -- Validate register model.
    { nickname, password } <- validateRegistration content

    -- Generate password hash.
    hash <- generateHash password

    -- Generate session token.
    token <- Token.generate

    id <- pool # transaction \client -> do
        -- Add player to database.
        id <- addPlayer client { nickname, hash }

        -- Add session to database.
        createSession id token client

        pure id

    pure $ noContent $ setCookieHeaderFull deployment { id: Id id, nickname, token }
