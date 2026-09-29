module TeamTavern.Server.Password.ForgotPassword (forgotPassword) where

import Prelude

import Async (Async)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Routes.Password.ForgotPassword as ForgotPassword
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, deliver)
import TeamTavern.Server.Infrastructure.GenerateNonce (Nonce, generateNonce, toString)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

type Player = {email :: String, nickname :: String}

queryString :: Query
queryString = Query """
    with inserted as (
        insert into password_reset (player_id, nonce)
        select player.id, $2
        from player
        where lower(player.email) = lower($1)
            and player.password_hash is not null
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

resetEmail :: Player -> Nonce -> Email
resetEmail { email, nickname } nonce =
    { to: email
    , subject: "Password reset"
    , blocks:
        [ Paragraph $ "Hi " <> nickname <> ","
        , Paragraph "Choose a new password for your TeamTavern account:"
        , Button { label: "Reset password", path: "/reset-password?nonce=" <> toString nonce }
        , Note "If you haven't made a password reset request, please ignore this email."
        ]
    , unsubscribe: true
    }

forgotPassword
    :: forall left
    .  Mailer
    -> Pool
    -> ForgotPassword.RequestContent
    -> Async left _
forgotPassword mailer pool {email} =
    sendResponse "Error sending password reset email" do
    -- Generate password reset nonce.
    nonce <- generateNonce

    -- Save password reset nonce.
    player <- addPasswordReset pool email nonce

    -- Send password reset email. The player waits for it, so a failed send
    -- fails the request.
    deliver mailer $ resetEmail player nonce

    pure noContent_
