module Postgres.Async.Query where

import Prelude

import Async (Async, fromEitherCont)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query, QueryParameter)
import Postgres.Query as Postgres
import Postgres.Result (Result)

query :: forall querier. Querier querier =>
    Query -> Array QueryParameter -> querier -> Async Error Result
query queryString parameters pool = fromEitherCont \callback ->
    Postgres.query queryString parameters callback pool

query_ :: forall querier. Querier querier =>
    Query -> querier -> Async Error Result
query_ queryString pool = fromEitherCont \callback ->
    Postgres.query_ queryString callback pool

execute :: forall querier. Querier querier =>
    Query -> Array QueryParameter -> querier -> Async Error Unit
execute queryString parameters pool = fromEitherCont \callback ->
    Postgres.execute queryString parameters callback pool

execute_ :: forall querier. Querier querier =>
    Query -> querier -> Async Error Unit
execute_ queryString pool = fromEitherCont \callback ->
    Postgres.execute_ queryString callback pool
