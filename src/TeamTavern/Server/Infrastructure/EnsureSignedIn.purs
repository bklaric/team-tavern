module TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo)
import TeamTavern.Server.Infrastructure.Postgres (reportDatabaseError)

type EnsureSignedInError errors = Variant
    ( internal :: Array String
    , client :: Array String
    | errors
    )

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
    Nothing -> Async.left $ inj (SProxy :: SProxy "client")
        [ "No cookie info has been found in cookies: " <> show cookies]
    Just cookieInfo @ { id, nickname, token } -> do
        result <- querier # query queryString (id : nickname :| token) # reportDatabaseError
        if rowCount result == 0
        then Async.left $ inj (SProxy :: SProxy "client")
            [ "Client session in cookies is invalid: " <> show cookies ]
        else Async.right cookieInfo
