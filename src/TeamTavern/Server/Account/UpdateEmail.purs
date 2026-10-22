module TeamTavern.Server.Account.UpdateEmail (createUpdateEmailLimit, updateEmail) where

import Prelude

import Async (Async, foreach, fromEffect)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Variant (Variant)
import Effect (Effect)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, NotAuthorizedRow_, TooManyRequestsRow, internal__, noContent_)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Routes.Account.UpdateEmail as UpdateEmail
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Account.Infrastructure.EmailTaken (emailTakenOrInternal)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter, countOne, createLimiter, ensureRoom)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail (validateEmail')
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Password.Infrastructure.DeleteResets (deleteResets)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (ConfirmationLimits, addConfirmation, sendConfirmation)
import Type.Row (type (+))
import Yoga.JSON.Async (read)

-- An address that differs from the account's only in case is the same
-- address, and is left as it is, confirmed or not.
queryString :: Query
queryString = Query """
    update player
    set email = $2, email_confirmed = false
    where id = $1 and (email is null or lower(email) <> lower($2))
    returning nickname
    """

-- | Each change mails a link to the new address, so an account changes its
-- | email 5 times an hour. Only a change that went through counts.
createUpdateEmailLimit :: Effect Limiter
createUpdateEmailLimit = createLimiter "Email changes of one account" { count: 5, seconds: 3600 }

updateEmail :: ∀ left. Limiter -> ConfirmationLimits -> Mailer -> Pool -> Cookies -> UpdateEmail.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow UpdateEmail.BadContent + NotAuthorizedRow_ + TooManyRequestsRow Wait + InternalRow_ + ()))
updateEmail limit confirmationLimits mailer pool cookies { email } =
    sendResponse "Error updating email" do
    { id } <- ensureSignedIn pool cookies
    email' <- validateEmail' email <#> Email.toString
    let playerId = unwrap id
    ensureRoom limit (show playerId) "Too many email changes of one account."
    confirmation <- pool # transaction \client -> do
        result <- client # query queryString (playerId :| email') # lmap (emailTakenOrInternal email')
        changed <- for (result # rows # head) \row ->
            (read row :: _ _ { nickname :: String })
            # lmap \error -> Terror internal__ [ "Error reading nickname: " <> show error ]
        for changed \{ nickname } -> do
            deleteResets client playerId
            addConfirmation client playerId email' <#> { email: email', nickname, nonce: _ }
    foreach confirmation \confirmation' -> do
        fromEffect $ countOne limit $ show playerId
        sendConfirmation confirmationLimits mailer confirmation'
    pure noContent_
