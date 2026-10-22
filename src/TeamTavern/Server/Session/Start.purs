module TeamTavern.Server.Session.Start where

import Prelude

import Async (Async, foreach)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Variant (Variant, match)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, TooManyRequestsRow, noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Bcrypt (Bcrypt)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeader)
import TeamTavern.Server.Infrastructure.DiscordSignIn (DiscordClient, exchangeDiscordCode)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Environment (Environment)
import TeamTavern.Server.Infrastructure.GoogleSignIn (GoogleClient, exchangeGoogleCode)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ResolveSteamId (SteamApi)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.SteamOpenId (SteamOpenIdUrl, verifySteamReturn)
import TeamTavern.Server.Player.Domain.Provider (Provider(..))
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (ConfirmationLimits, sendConfirmation)
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession)
import TeamTavern.Server.Session.Start.CheckPassword (SignInLimits, checkPassword, confirmPassword)
import TeamTavern.Server.Session.Start.CheckProvider (checkProvider)
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import TeamTavern.Server.Session.Start.Unknown (unknownDiscord, unknownGoogle, unknownSteam)
import Type.Row (type (+))

-- | `clientAddress` is the address of the client that asks, and `origin` the
-- | one it asked on.
start :: ∀ left.
    Environment -> SignInLimits -> ConfirmationLimits -> Bcrypt -> Mailer -> DiscordClient -> SteamOpenIdUrl -> SteamApi -> GoogleClient
    -> Pool -> Cookies -> String -> String -> StartSession.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow StartSession.BadContent + TooManyRequestsRow Wait + InternalRow_ + ()))
start environment signInLimits confirmationLimits bcrypt mailer discordClient steamOpenIdUrl steamApi googleClient pool cookies clientAddress origin body =
    sendResponse "Error starting session" do
    -- Generate session token.
    token <- Token.generate

    -- Replace the session the browser holds, if it holds one.
    let startFor client { id, confirmation } = do
            revokeSession client cookies
            createSession id token client
            pure { confirmation }

        -- The player the provider vouched for signs in. One new to the site is
        -- answered by `unknown` once the transaction is over.
        startWith provider providerId email unknown = do
            started <- pool # transaction \client ->
                checkProvider provider client providerId email >>= traverse (startFor client)
            maybe unknown pure started

    -- Discord, Steam and Google are asked, and the password is compared,
    -- before the transaction, which holds a connection from the pool until it
    -- ends.
    {confirmation} <- body # match
        { password: \bodyEmail -> do
            checked <- checkPassword signInLimits bcrypt clientAddress bodyEmail pool
            pool # transaction \client -> do
                confirmPassword checked client
                startFor client { id: checked.id, confirmation: Nothing }
        , discord: \{code} -> do
            discordUser <- exchangeDiscordCode discordClient origin code
            startWith Discord discordUser.discordId discordUser.email $ unknownDiscord pool discordUser
        , steam: \{assertion} -> do
            steamId <- verifySteamReturn steamOpenIdUrl origin pool assertion
            startWith Steam steamId Nothing $ unknownSteam steamApi pool steamId
        , google: \{code} -> do
            googleUser <- exchangeGoogleCode googleClient origin code
            startWith Google googleUser.googleId googleUser.email $ unknownGoogle pool googleUser
        }

    foreach confirmation $ sendConfirmation confirmationLimits mailer

    pure $ noContent $ setCookieHeader environment token
