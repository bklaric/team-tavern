module TeamTavern.Server.Player.ResendConfirmation (resendConfirmation) where

import Prelude

import Async (Async, foreach)
import Data.Variant (Variant)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import Jarilo (InternalRow_, NoContentRow_, NotAuthorizedRow_, TooManyRequestsRow, noContent_)
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe)
import TeamTavern.Server.Infrastructure.RateLimit (ensureAdmission)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (ConfirmationLimits, addConfirmation, trySendConfirmation)
import Type.Row (type (+))

-- A confirmed or missing address has nothing to send.
queryString :: Query
queryString = Query """
    select player.id, player.nickname, player.email
    from player
    where player.id = $1
        and player.email is not null
        and not player.email_confirmed
    """

-- | The player asked for the link, so one past its mailbox's limits is
-- | refused, with how long to wait. Its link is added either way, and works
-- | once the wait is over and it is sent again.
resendConfirmation :: ∀ left. ConfirmationLimits -> Mailer -> Pool -> Cookies
    -> Async left (Variant (NoContentRow_ + NotAuthorizedRow_ + TooManyRequestsRow Wait + InternalRow_ + ()))
resendConfirmation limits mailer pool cookies =
    sendResponse "Error resending email confirmation" do
    {id} <- ensureSignedIn pool cookies
    player :: _ {id :: Int, nickname :: String, email :: String} <-
        queryFirstMaybe pool queryString (id : [])
    foreach player \{id: playerId, nickname, email} -> do
        nonce <- addConfirmation pool playerId email
        admission <- trySendConfirmation limits mailer {email, nickname, nonce}
        ensureAdmission (pure admission) "Too many confirmation links to one mailbox."
    pure noContent_
