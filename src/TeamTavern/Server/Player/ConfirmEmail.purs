module TeamTavern.Server.Player.ConfirmEmail (confirmEmail) where

import Prelude

import Async (Async)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Player.ConfirmEmail as ConfirmEmail
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- A link confirms the address it was sent to and nothing else: it is used up
-- either way, and confirms only while the player's email is still that address.
-- One that confirmed nothing is answered as not found.
-- It doesn't expire, since until it is clicked the site sends the address
-- nothing else, and works signed out, from whatever browser opens the email.
queryString :: Query
queryString = Query """
    with consumed as (
        update email_confirmation
        set consumed = true
        where nonce = $1 and consumed = false
        returning player_id, email
    ),
    confirmed as (
        update player
        set email_confirmed = true
        from consumed
        where player.id = consumed.player_id
            and lower(player.email) = lower(consumed.email)
        returning player.id
    )
    select id as "playerId" from confirmed
    """

confirmEmail :: ∀ left. Pool -> ConfirmEmail.RequestContent -> Async left _
confirmEmail pool {nonce} =
    sendResponse "Error confirming email" do
    (_ :: {playerId :: Int}) <- queryFirstNotFound pool queryString (nonce : [])
    pure noContent_
