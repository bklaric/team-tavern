module TeamTavern.Server.Password.ForgotPassword (forgotPassword) where

import Prelude

import Async (Async)
import Effect.Class.Console (logShow)
import Jarilo (noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:|))
import Sendgrid (Message, sendAsync)
import TeamTavern.Routes.Password.ForgotPassword as ForgotPassword
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Deployment (Deployment(..))
import TeamTavern.Server.Infrastructure.EnsureNotSignedIn (ensureNotSignedIn)
import TeamTavern.Server.Infrastructure.GenerateNonce (Nonce, generateNonce, toString)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

type Player = {email :: String, nickname :: String}

queryString :: Query
queryString = Query """
    with inserted as (
        insert into password_reset (player_id, nonce)
        select player.id, $2
        from player
        where lower(player.email) = lower($1)
        returning player_id
    )
    select player.email, player.nickname
    from player
        join inserted on inserted.player_id = player.id
    """

addPasswordReset
    :: forall errors
    .  Pool
    -> String
    -> Nonce
    -> Async (LoadSingleError errors) Player
addPasswordReset pool email nonce = do
    queryFirstNotFound pool queryString (email :| nonce)

message :: Deployment -> Player -> Nonce -> Message
message deployment { email, nickname } nonce = let
    link = case deployment of
        Local -> "https://localhost/reset-password?nonce=" <> toString nonce
        Cloud -> "https://www.teamtavern.net/reset-password?nonce=" <> toString nonce
    in
    { to: email
    , from: "TeamTavern admin@teamtavern.net"
    , subject: "Password reset"
    , html: "Hi " <> nickname <> ",<br /><br />"
        <> "Open the link below to reset your TeamTavern account password:<br /><br />"
        <> "<a href=\"" <> link <> "\">" <> link <> "</a><br /><br />"
        <> "If you haven't made a password reset request, please ignore this email."
    , text: "Hi " <> nickname <> ",\n"
        <> "Open the link below to reset your TeamTavern account password:\n"
        <> link <> "\n"
        <> "If you haven't made a password reset request, please ignore this email."
    }

sendPasswordResetEmail :: forall errors.
    Deployment -> Player -> Nonce -> Async (InternalTerror_ errors) Unit
sendPasswordResetEmail deployment player nonce =
    case deployment of
    Local -> logShow $ message deployment player nonce
    Cloud -> sendAsync $ message deployment player nonce

forgotPassword
    :: forall left
    .  Deployment
    -> Pool
    -> Cookies
    -> ForgotPassword.RequestContent
    -> Async left _
forgotPassword deployment pool cookies {email} =
    sendResponse "Error sending password reset email" do
    -- Ensure user is not signed in.
    ensureNotSignedIn cookies

    -- Generate password reset nonce.
    nonce <- generateNonce

    -- Save password reset nonce.
    player <- addPasswordReset pool email nonce

    -- Send password reset email.
    sendPasswordResetEmail deployment player nonce

    pure noContent_
