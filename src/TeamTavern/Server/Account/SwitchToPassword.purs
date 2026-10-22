module TeamTavern.Server.Account.SwitchToPassword (switchToPassword) where

import Prelude

import Async (Async, foreach, left)
import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Traversable (for, traverse)
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, NotAuthorizedRow_, badRequest_, noContent_)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Account.SwitchToPassword as SwitchToPassword
import TeamTavern.Server.Account.Infrastructure.EmailTaken (emailTakenOrInternal)
import TeamTavern.Server.Infrastructure.Bcrypt (Bcrypt)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryNone, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail (validateEmail')
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Password.Infrastructure.DeleteResets (deleteResets)
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Password (validatePassword')
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (ConfirmationLimits, addConfirmation, sendConfirmation)
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeOtherSessions)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

heldQuery :: Query
heldQuery = Query """
    select nickname, email, email_confirmed as "emailConfirmed" from player where id = $1 for update
    """

-- Anyone can sign up with an address they don't own, so a confirmed address
-- that takes a password takes it from a password account that never confirmed
-- it, with the links mailed to it. That account keeps its posts and sessions,
-- and its account page asks for another address.
freeQuery :: Query
freeQuery = Query """
    with freed as (
        update player
        set email = null
        where lower(email) = lower($2) and id <> $1
            and password_hash is not null and not email_confirmed
        returning id
    ),
    resets as (
        delete from password_reset using freed where password_reset.player_id = freed.id
    )
    delete from email_confirmation using freed where email_confirmation.player_id = freed.id
    """

-- An account without an address takes the one given, unconfirmed.
switchQuery :: Query
switchQuery = Query """
    update player
    set password_hash = $2,
        discord_id = null,
        steam_sign_in_id = null,
        google_id = null,
        email = coalesce(email, $3::text),
        email_confirmed = email_confirmed and email is not null
    where id = $1
    """

switchToPassword :: ∀ left. ConfirmationLimits -> Bcrypt -> Mailer -> Pool -> Cookies -> SwitchToPassword.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow SwitchToPassword.BadContent + NotAuthorizedRow_ + InternalRow_ + ()))
switchToPassword confirmationLimits bcrypt mailer pool cookies { password, email } =
    sendResponse "Error switching to a password" do
    { id, token } <- ensureSignedIn pool cookies
    password' <- validatePassword' password
    given <- traverse (validateEmail' >>> map Email.toString) email
    hash <- generateHash bcrypt password'
    let playerId = unwrap id
    confirmation <- pool # transaction \client -> do
        held :: { nickname :: String, email :: Maybe String, emailConfirmed :: Boolean } <-
            queryFirstInternal client heldQuery (playerId : [])
        taken <- case held.email, given of
            Just _, _ -> pure Nothing
            Nothing, Just email' -> pure $ Just email'
            Nothing, Nothing -> left $ Terror (badRequest_ $ inj (Proxy :: _ "email") {})
                [ "A password needs an email to sign in with, and the account has none." ]
        case held.email of
            Just email' | held.emailConfirmed -> queryNone client freeQuery (playerId :| email')
            _ -> pure unit
        void $ client # query switchQuery (playerId : hash :| toNullable taken)
            # lmap (emailTakenOrInternal $ fromMaybe "" $ held.email <|> taken)
        revokeOtherSessions client playerId token
        deleteResets client playerId
        for taken \email' -> addConfirmation client playerId email' <#>
            { email: email', nickname: held.nickname, nonce: _ }
    foreach confirmation $ sendConfirmation confirmationLimits mailer
    pure noContent_
