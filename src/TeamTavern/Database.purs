module TeamTavern.Database where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either (Either)
import Effect (Effect)
import Node.Errors (Error)
import Postgres.Query (class Querier, Query, QueryParameter)
import Postgres.Query as Postgres
import Postgres.Result (Result)

query
    :: forall querier
    .  Querier querier
    => Query
    -> Array QueryParameter
    -> querier
    -> ContT Unit Effect (Either Error Result)
query queryString parameters pool = ContT $ \callback ->
    Postgres.query queryString parameters callback pool
