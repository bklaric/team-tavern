module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn) where

import Prelude

import Async (Async, left, right)
import Data.Maybe (Maybe(..), fromMaybe)
import Jarilo (InternalRow_, NotAuthorizedRow_, notAuthorized, notAuthorized__)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import JavaScript.Npm.Pg.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo, removeCookieHeader)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (reportDatabaseError)
import Type.Row (type (+))

type EnsureSignedInError errors = TerrorVar (InternalRow_ + NotAuthorizedRow_ + errors)

queryString :: Query
queryString = Query """
    select session.id
    from session
    join player on player.id = session.player_id
    where player.id = $1
        and lower(player.nickname) = lower($2)
        and session.token = $3
        and revoked = false
    """

-- A session the server refuses has its cookies removed, so the client, which
-- reads the id and nickname cookies to tell whether the player is signed in,
-- sees them signed out from then on.
ensureSignedIn :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async (EnsureSignedInError errors) CookieInfo
ensureSignedIn querier cookies =
    case lookupCookieInfo cookies of
    Nothing -> left $ Terror notAuthorized__
        [ "No cookie info has been found in cookies: " <> show cookies]
    Just cookieInfo @ { id, nickname, token } -> do
        result <- querier # query queryString (id : nickname :| token) # reportDatabaseError
        if fromMaybe 0 (rowCount result) == 0
        then left $ Terror (notAuthorized removeCookieHeader unit)
            [ "Client session in cookies is invalid: " <> show cookieInfo ]
        else right cookieInfo
