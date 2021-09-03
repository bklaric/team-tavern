module TeamTavern.Server.Infrastructure.EnsureSignedInOwner where

import Prelude

import Async (Async)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (EnsureSignedInError, ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotAuthorized)

type EnsureSignedInOwnerError errors = EnsureSignedInError
    ( notAuthorized :: Array String
    | errors
    )

queryString :: Query
queryString = Query """
    select team.id
    from team
    join player on player.id = team.owner_id
    where player.id = $1 and team.handle = $2
    """

ensureSignedInOwner :: forall errors querier. Querier querier =>
    querier -> Cookies -> String -> Async (EnsureSignedInOwnerError errors) { cookieInfo :: CookieInfo, teamId :: Int }
ensureSignedInOwner querier cookies handle = do
    cookieInfo <- ensureSignedIn querier cookies
    { id } :: { id :: Int } <- queryFirstNotAuthorized querier queryString (cookieInfo.id :| handle)
    pure { cookieInfo, teamId: id }
