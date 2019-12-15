module TeamTavern.Server.Password.Forgot where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Maybe (Maybe)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Password.Forgot.AddPasswordReset (addPasswordReset)
import TeamTavern.Server.Password.Forgot.LogError (logError)
import TeamTavern.Server.Password.Forgot.ReadEmailAddress (readEmailAddress)
import TeamTavern.Server.Password.Forgot.SendPasswordResetEmail (sendPasswordResetEmail)
import TeamTavern.Server.Password.Forgot.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Nonce as Nonce

forgot
    :: Pool
    -> Maybe Client
    -> Cookies
    -> Body
    -> (forall left. Async left Response)
forgot pool client cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure user is not signed in.
    ensureNotSignedIn cookies

    -- Read email address from body.
    email <- readEmailAddress body

    -- Generate password reset nonce.
    nonce <- Nonce.generate

    -- Save password reset nonce.
    player <- addPasswordReset pool email nonce

    -- Send password reset email.
    sendPasswordResetEmail client player nonce
