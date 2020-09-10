module TeamTavern.Server.Player.Register where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Player.Domain.Hash as Hash
import TeamTavern.Server.Player.Domain.Nonce as Nonce
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.LogError (logError)
import TeamTavern.Server.Player.Register.ReadDto (readDto)
import TeamTavern.Server.Player.Register.SendEmail (sendEmail)
import TeamTavern.Server.Player.Register.SendResponse (sendResponse)
import TeamTavern.Server.Player.Register.ValidateModel (validateModel)

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
    hash <- Hash.generate password

    -- Generate email confirmation nonce.
    nonce <- Nonce.generate

    -- Add player to database.
    _ <- addPlayer pool { email, nickname, hash, nonce }

    -- Send confirmation email.
    sendEmail client { email, nickname, nonce }

    pure { email, nickname }
