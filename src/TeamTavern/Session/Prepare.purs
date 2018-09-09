module TeamTavern.Session.Prepare where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import TeamTavern.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Player.Infrastructure.GenerateSecrets (generateSecrets)
import TeamTavern.Player.Infrastructure.ReadIdentifiers (readIdentifiers)
import TeamTavern.Session.Prepare.CreateSession (createSession)
import TeamTavern.Session.Prepare.LogError (logError)
import TeamTavern.Session.Prepare.Response (response)
import TeamTavern.Session.Prepare.SendSignInEmail (sendSignInEmail)

prepare :: forall left.
    Pool -> Maybe Client -> Map String String -> Body -> Async left Response
prepare pool client cookies body =
    response $ examineLeftWithEffect logError do
    -- Ensure player isn't signed in.
    ensureNotSignedIn cookies

    -- Read identifiers from body.
    { email, nickname } <- readIdentifiers body

    -- Generate secrets.
    { nonce, token } <- generateSecrets

    -- Create session.
    createSession pool { email, nickname, token, nonce }

    -- Send sign in email.
    sendSignInEmail client { email, nickname, nonce }
