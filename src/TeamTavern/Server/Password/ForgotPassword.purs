module TeamTavern.Server.Password.ForgotPassword (ForgotPasswordLimits, createForgotPasswordLimits, forgotPassword) where

import Prelude

import Async (Async)
import Data.Map (Map)
import Data.Variant (Variant)
import Effect (Effect)
import Jarilo (InternalRow_, NoContentRow_, NotFoundRow_, TooManyRequestsRow, noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Password.ForgotPassword as ForgotPassword
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, deliver)
import TeamTavern.Server.Infrastructure.GenerateNonce (Nonce, generateNonce, toString)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.RateLimit (ClientLimits, Limiter, clientAddress, createClientLimiter, createLimiter, ensureAdmitted)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Session.Domain.Token (Token(..))
import TeamTavern.Server.Session.Domain.Token as Token
import Type.Row (type (+))

type Player = {id :: Int, email :: String, nickname :: String}

playerQuery :: Query
playerQuery = Query """
    select player.id, player.email, player.nickname
    from player
    where lower(player.email) = lower($1)
        and player.password_hash is not null
    """

-- Each new link clears out those past their hour.
insertQuery :: Query
insertQuery = Query """
    with stale as (
        delete from password_reset where created < now() - interval '1 hour'
    )
    insert into password_reset (player_id, nonce_hash)
    values ($1, $2)
    """

addPasswordReset
    :: forall errors
    .  Pool
    -> Int
    -> Nonce
    -> Async (InternalTerror_ errors) Unit
addPasswordReset pool playerId nonce = do
    nonceHash <- Token.hash $ Token $ toString nonce
    queryNone pool insertQuery (playerId :| nonceHash)

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

-- | Anyone can ask for a link to any address, so a client gets 10 tries an
-- | hour, and an account 3 links an hour, however many clients ask for them.
-- | An account is counted by its id: an address can be written many ways
-- | that Postgres takes for the same, and one with no account is no key.
type ForgotPasswordLimits = { perClient :: Limiter, perAccount :: Limiter }

createForgotPasswordLimits :: ClientLimits -> Effect ForgotPasswordLimits
createForgotPasswordLimits clientLimits = do
    perClient <- createClientLimiter clientLimits "Reset links from one client" { count: 10, seconds: 3600 }
    perAccount <- createLimiter "Reset links to one account" { count: 3, seconds: 3600 }
    pure { perClient, perAccount }

forgotPassword
    :: forall left
    .  ForgotPasswordLimits
    -> Mailer
    -> Pool
    -> Map String String
    -> ForgotPassword.RequestContent
    -> Async left (Variant (NoContentRow_ + NotFoundRow_ + TooManyRequestsRow Wait + InternalRow_ + ()))
forgotPassword limits mailer pool headers {email} =
    sendResponse "Error sending password reset email" do
    ensureAdmitted limits.perClient (clientAddress headers) "Too many reset links from one client."

    player :: Player <- queryFirstNotFound pool playerQuery (email : [])
    ensureAdmitted limits.perAccount (show player.id) "Too many reset links to one account."

    -- Generate password reset nonce.
    nonce <- generateNonce

    -- Save password reset nonce.
    addPasswordReset pool player.id nonce

    -- Send password reset email. The player waits for it, so a failed send
    -- fails the request.
    deliver mailer $ resetEmail player nonce

    pure noContent_
