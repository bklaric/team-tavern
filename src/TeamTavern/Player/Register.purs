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
import TeamTavern.Infrastructure.GenerateHash (generateHash')
import TeamTavern.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Player.Register.GenerateNonce (generateNonce)
import TeamTavern.Player.Register.LogError (logError)
import TeamTavern.Player.Register.ReadDto (readDto)
import TeamTavern.Player.Register.SendEmail (sendEmail)
import TeamTavern.Player.Register.SendResponse (sendResponse)
import TeamTavern.Player.Register.ValidateModel (unPassword, validateModel)

register :: forall left.
    Pool -> Maybe Client -> Map String String -> Body -> Async left Response
register pool client cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure not signed in.
    ensureNotSignedIn cookies

    -- Read register dto.
    dto <- readDto body

    -- Validate register model.
    model @ { email, nickname, password } <- validateModel dto

    -- Generate password hash.
    hash <- generateHash' unPassword password

    -- Generate email confirmation nonce.
    nonce <- generateNonce

    -- Add player to database.
    addPlayer pool { email, nickname, hash, nonce }

    -- Send confirmation email.
    sendEmail client { email, nickname, nonce }

    pure { email, nickname }
