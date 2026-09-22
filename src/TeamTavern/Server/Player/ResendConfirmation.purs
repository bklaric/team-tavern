module TeamTavern.Server.Player.ResendConfirmation (resendConfirmation) where

import Prelude

import Async (Async, foreach)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import Jarilo (noContent_)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (addConfirmation, sendConfirmation)

-- A confirmed or missing address has nothing to send.
queryString :: Query
queryString = Query """
    select player.id, player.nickname, player.email
    from player
    where player.id = $1
        and player.email is not null
        and not player.email_confirmed
    """

resendConfirmation :: ∀ left. Deployment -> Pool -> Cookies -> Async left _
resendConfirmation deployment pool cookies =
    sendResponse "Error resending email confirmation" do
    {id} <- ensureSignedIn pool cookies
    player :: _ {id :: Int, nickname :: String, email :: String} <-
        queryFirstMaybe pool queryString (id : [])
    foreach player \{id: playerId, nickname, email} -> do
        nonce <- addConfirmation pool playerId email
        sendConfirmation deployment {email, nickname, nonce}
    pure noContent_
