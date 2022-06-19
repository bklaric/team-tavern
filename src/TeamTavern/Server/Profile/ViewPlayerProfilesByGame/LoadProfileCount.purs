module TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfileCount where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Types (Timezone, Handle)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (queryStringWithoutPagination)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (read)

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
        # label (Proxy :: _ "databaseError")
    count <- rows result
        # Array.head
        # Async.note (inj (Proxy :: _ "noRowsSomehow") result)
    count' :: LoadProfileCountResult <- read count
        # labelMap (Proxy :: _ "unreadableCount") { count, errors: _ }
    pure count'.count
