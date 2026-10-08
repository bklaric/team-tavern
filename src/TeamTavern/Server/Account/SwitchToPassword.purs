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
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail (validateEmail')
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Player.Domain.Hash (generateHash)
import TeamTavern.Server.Player.Domain.Password (validatePassword')
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (addConfirmation, sendConfirmation)
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeOtherSessions)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

heldQuery :: Query
heldQuery = Query """
    select nickname, email from player where id = $1 for update
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

switchToPassword :: ∀ left. Mailer -> Pool -> Cookies -> SwitchToPassword.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow SwitchToPassword.BadContent + NotAuthorizedRow_ + InternalRow_ + ()))
switchToPassword mailer pool cookies { password, email } =
    sendResponse "Error switching to a password" do
    { id, token } <- ensureSignedIn pool cookies
    password' <- validatePassword' password
    given <- traverse (validateEmail' >>> map Email.toString) email
    hash <- generateHash password'
    let playerId = unwrap id
    confirmation <- pool # transaction \client -> do
        held :: { nickname :: String, email :: Maybe String } <-
            queryFirstInternal client heldQuery (playerId : [])
        taken <- case held.email, given of
            Just _, _ -> pure Nothing
            Nothing, Just email' -> pure $ Just email'
            Nothing, Nothing -> left $ Terror (badRequest_ $ inj (Proxy :: _ "email") {})
                [ "A password needs an email to sign in with, and the account has none." ]
        void $ client # query switchQuery (playerId : hash :| toNullable taken)
            # lmap (emailTakenOrInternal $ fromMaybe "" $ held.email <|> taken)
        revokeOtherSessions client playerId token
        for taken \email' -> addConfirmation client playerId email' <#>
            { email: email', nickname: held.nickname, nonce: _ }
    foreach confirmation $ sendConfirmation mailer
    pure noContent_
