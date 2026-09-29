module TeamTavern.Server.Player.Register (register) where

import Prelude

import Async (Async, foreach)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeader)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Environment (Environment)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl, discordEmail, fetchDiscordUser)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (addConfirmation, sendConfirmation)
import TeamTavern.Server.Player.Register.AddPlayer (addPlayer)
import TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord)
import TeamTavern.Server.Player.Register.ValidateRegistration (validateRegistration)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession)
import TeamTavern.Server.Session.Start.CreateSession (createSession)

register :: ∀ left.
    Environment -> Mailer -> DiscordApiUrl -> Pool -> Cookies -> RegisterPlayer.RequestContent -> Async left _
register environment mailer discordApiUrl pool cookies content =
    sendResponse "Error registering player" do
    -- Validate register model.
    registration <- validateRegistration content

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

        , discord: \{nickname, accessToken} -> do
            discordUser <- fetchDiscordUser discordApiUrl accessToken
            pool # transaction \client -> do
                id <- addPlayerDiscord client nickname discordUser
                revokeSession client cookies
                createSession id token client

                -- Discord confirms an address it verified; any other gets the link.
                confirmation <- case discordEmail discordUser of
                    Just {email, confirmed: false} -> do
                        nonce <- addConfirmation client id email
                        pure $ Just {email, nickname: unwrap nickname, nonce}
                    _ -> pure Nothing

                pure {confirmation}
        }

    foreach confirmation $ sendConfirmation mailer

    pure $ noContent $ setCookieHeader environment token
