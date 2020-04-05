module TeamTavern.Server.Profile.ViewGamePlayers.LoadProfileCount where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.Routes (Filters, Handle, Timezone)
import TeamTavern.Server.Profile.ViewGamePlayers.LoadProfiles (queryStringWithoutPagination)

type LoadProfileCountResult = { count :: Int }

type LoadProfileCountError errors = Variant
    ( unreadableCount ::
        { count :: Foreign
        , errors :: MultipleErrors
        }
    , noRowsSomehow :: Result
    , databaseError :: Error
    | errors )

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
    :: forall errors
    .  Client
    -> Handle
    -> Timezone
    -> Filters
    -> Async (LoadProfileCountError errors) Int
loadProfileCount client handle timezone filters = do
    result <- client
        # query (queryString handle timezone filters) []
        # label (SProxy :: SProxy "databaseError")
    count <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "noRowsSomehow") result)
    count' :: LoadProfileCountResult <- read count
        # labelMap (SProxy :: SProxy "unreadableCount") { count, errors: _ }
    pure count'.count
