module TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfileCount where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..))
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Types (Timezone, Handle)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (queryStringWithoutPagination)

queryString :: Handle -> Timezone -> Filters -> Query
queryString handle timezone filters = let
    Query profilesQueryString =
        queryStringWithoutPagination handle timezone filters
    in
    Query $ """
    select count(*)::int as "count"
    from (
        """ <> profilesQueryString <> """
    ) as profiles"""

loadProfileCount
    :: ∀ errors
    .  Client
    -> Handle
    -> Timezone
    -> Filters
    -> Async (InternalTerror_ errors) Int
loadProfileCount client handle timezone filters = do
    { count } :: { count :: Int } <-
        queryFirstInternal_ client (queryString handle timezone filters)
    pure count
