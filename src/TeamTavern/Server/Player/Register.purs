module TeamTavern.Server.Player.Register (createRegisterLimit, register) where

import Prelude

import Async (Async, foreach)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj, match)
import Effect (Effect)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, TooManyRequestsRow, noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeader)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Environment (Environment)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.RateLimit (ClientLimits, Limiter, createClientLimiter, ensureAdmitted)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Provider (Provider(..))
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (ConfirmationLimits, addConfirmation, sendConfirmation)
import TeamTavern.Server.Player.Infrastructure.SignInTicket (takeSignInTicket)
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord)
import TeamTavern.Server.Player.Register.AddPlayerGoogle (addPlayerGoogle)
import TeamTavern.Server.Player.Register.AddPlayerSteam (addPlayerSteam)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistration)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession)
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | A client tries 10 registrations an hour. Each that passes validation
-- | counts, whether or not it makes an account: a password's runs bcrypt, and
-- | any can answer that a nickname or an address is taken.
createRegisterLimit :: ClientLimits -> Effect Limiter
createRegisterLimit clientLimits =
    createClientLimiter clientLimits "Registrations from one client" { count: 10, seconds: 3600 }

register :: ∀ left.
    Environment -> Limiter -> ConfirmationLimits -> Mailer -> Pool -> Cookies -> String -> RegisterPlayer.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow RegisterPlayer.BadContent + TooManyRequestsRow Wait + InternalRow_ + ()))
register environment limit confirmationLimits mailer pool cookies clientAddress content =
    sendResponse "Error registering player" do
    -- Validate register model.
    registration <- validateRegistration content

    ensureAdmitted limit clientAddress "Too many registrations from one client."

    -- Generate session token.
    token <- Token.generate

    {confirmation} <- registration # match
        { password: \{email, nickname, password} -> do
            -- Generate password hash.
            hash <- generateHash password

            pool # transaction \client -> do
                -- Add player to database.
                id <- addPlayer client { email, nickname, hash }

                -- Add session to database, replacing the one the browser holds.
                revokeSession client cookies
                createSession id token client

                -- A typed address is confirmed by the link the site emails.
                nonce <- addConfirmation client id (Email.toString email)

                pure {confirmation: Just
                    {email: Email.toString email, nickname: unwrap nickname, nonce}}

        , discord: \{nickname, ticket} -> pool # transaction \client -> do
            -- A failed registration takes back the spent ticket with the rest.
            ticketed <- takeSignInTicket client Discord (inj (Proxy :: _ "discordTicket") {}) ticket
            id <- addPlayerDiscord client nickname ticketed
            revokeSession client cookies
            createSession id token client

            -- Discord confirms an address it verified; any other gets the link.
            confirmation <- case ticketed.email of
                Just {email, confirmed: false} -> do
                    nonce <- addConfirmation client id email
                    pure $ Just {email, nickname: unwrap nickname, nonce}
                _ -> pure Nothing

            pure {confirmation}

        , steam: \{nickname, email, ticket} -> pool # transaction \client -> do
            -- A failed registration takes back the spent ticket with the rest.
            { providerId: steamId } <- takeSignInTicket client Steam (inj (Proxy :: _ "steamTicket") {}) ticket
            id <- addPlayerSteam client { nickname, email, steamId }
            revokeSession client cookies
            createSession id token client

            -- Steam gives no address, so the one typed in gets the link.
            nonce <- addConfirmation client id (Email.toString email)

            pure {confirmation: Just
                {email: Email.toString email, nickname: unwrap nickname, nonce}}

        , google: \{nickname, ticket} -> pool # transaction \client -> do
            -- A failed registration takes back the spent ticket with the rest.
            ticketed <- takeSignInTicket client Google (inj (Proxy :: _ "googleTicket") {}) ticket
            id <- addPlayerGoogle client nickname ticketed
            revokeSession client cookies
            createSession id token client

            -- Google confirms an address it verified; any other gets the link.
            confirmation <- case ticketed.email of
                Just {email, confirmed: false} -> do
                    nonce <- addConfirmation client id email
                    pure $ Just {email, nickname: unwrap nickname, nonce}
                _ -> pure Nothing

            pure {confirmation}
        }

    foreach confirmation $ sendConfirmation confirmationLimits mailer

    pure $ noContent $ setCookieHeader environment token
