module TeamTavern.Server.Password.Reset where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Password.Reset.EnsureValidNonce (ensureValidNonce)
import TeamTavern.Server.Password.Reset.LogError (logError)
import TeamTavern.Server.Password.Reset.ReadNewPassword (readNewPassword)
import TeamTavern.Server.Password.Reset.SendResponse (sendResponse)
import TeamTavern.Server.Password.Reset.UpdatePassword (updatePassword)
import TeamTavern.Server.Password.Reset.ValidatePassword (validatePassword)
import TeamTavern.Server.Player.Domain.Hash (generateHash)

reset :: Pool -> Cookies  -> Body -> (forall left. Async left Response)
reset pool cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure user is not signed in.
    ensureNotSignedIn cookies

    -- Read email address from body.
    { password, nonce } <- readNewPassword body

    -- Validate password.
    validPassword <- validatePassword password

    -- Ensure nonce is valid.
    playerId <- ensureValidNonce pool nonce

    -- Generate password hash.
    hash <- generateHash validPassword

    -- Update the password.
    updatePassword pool playerId hash
