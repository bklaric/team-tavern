module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn) where

import Prelude

import Async (Async, left, right)
import Data.Maybe (Maybe(..))
import Jarilo (notAuthorized__)
import Postgres.Async.Query (query)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo)
import TeamTavern.Server.Infrastructure.Error (InternalRow_, NotAuthorizedRow_, TavernError(..))
import TeamTavern.Server.Infrastructure.Postgres (reportDatabaseError)
import Type.Row (type (+))

type EnsureSignedInError errors = TavernError (InternalRow_ + NotAuthorizedRow_ + errors)

queryString :: Query
queryString = Query """
    select session.id
    from session
    join player on player.id = session.player_id
    where player.id = $1
        and player.nickname = $2
        and session.token = $3
        and revoked = false
    """

ensureSignedIn :: forall querier errors. Querier querier =>
    querier -> Cookies -> Async (EnsureSignedInError errors) CookieInfo
ensureSignedIn querier cookies =
    case lookupCookieInfo cookies of
    Nothing -> left $ TavernError notAuthorized__
        [ "No cookie info has been found in cookies: " <> show cookies]
    Just cookieInfo @ { id, nickname, token } -> do
        result <- querier # query queryString (id : nickname :| token) # reportDatabaseError
        if rowCount result == 0
        then left $ TavernError notAuthorized__
            [ "Client session in cookies is invalid: " <> show cookies ]
        else right cookieInfo
