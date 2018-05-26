module Architecture.Postgres.Query where

import Async (Async, fromEitherCont)
import Node.Errors (Error)
import Postgres.Query (class Querier, Query, QueryParameter)
import Postgres.Query as Postgres
import Postgres.Result (Result)

query :: forall querier. Querier querier =>
    Query -> Array QueryParameter -> querier -> Async Error Result
query queryString parameters pool = fromEitherCont \callback ->
    Postgres.query queryString parameters callback pool
