module TeamTavern.Server.Account.UpdateEmail (updateEmail) where

import Prelude

import Async (Async, foreach)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Jarilo (internal__, noContent_)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Routes.Account.UpdateEmail as UpdateEmail
import TeamTavern.Server.Account.Infrastructure.EmailTaken (emailTakenOrInternal)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.ValidateEmail (validateEmail')
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (addConfirmation, sendConfirmation)
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

updateEmail :: ∀ left. Mailer -> Pool -> Cookies -> UpdateEmail.RequestContent -> Async left _
updateEmail mailer pool cookies { email } =
    sendResponse "Error updating email" do
    { id } <- ensureSignedIn pool cookies
    email' <- validateEmail' email <#> Email.toString
    let playerId = unwrap id
    confirmation <- pool # transaction \client -> do
        result <- client # query queryString (playerId :| email') # lmap (emailTakenOrInternal email')
        changed <- for (result # rows # head) \row ->
            (read row :: _ _ { nickname :: String })
            # lmap \error -> Terror internal__ [ "Error reading nickname: " <> show error ]
        for changed \{ nickname } -> addConfirmation client playerId email' <#> { email: email', nickname, nonce: _ }
    foreach confirmation $ sendConfirmation mailer
    pure noContent_
