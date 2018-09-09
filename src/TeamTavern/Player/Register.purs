module TeamTavern.Player.Register where

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
import TeamTavern.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Player.Register.LogError (logError)
import TeamTavern.Player.Register.Response (response)
import TeamTavern.Player.Register.SendEmail (sendRegistrationEmail)

register :: forall left.
    Pool -> Maybe Client -> Map String String -> Body -> Async left Response
register pool client cookies body =
    response $ examineLeftWithEffect logError do
    ensureNotSignedIn cookies
    { email, nickname } <- readIdentifiers body
    { token, nonce } <- generateSecrets
    let credentials = { email, nickname, token, nonce }
    addPlayer pool credentials
    sendRegistrationEmail client { email, nickname, nonce }
    pure { email, nickname }
