module TeamTavern.Architecture.Postgres.Pool where

import Prelude

import Async (Async, fromEffect, fromEitherCont, runAsync)
import Data.Options (Options)
import Postgres.Client (Client)
import Postgres.Client.Config (ClientConfig)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Pool (create) as Pool
import Postgres.Pool.Config (PoolConfig)
import Postgres.Pool.Transaction (withTransaction) as Pool

create :: forall left.
    Options PoolConfig -> Options ClientConfig -> Async left Pool
create poolConfig clientConfig =
    Pool.create poolConfig clientConfig # fromEffect

withTransaction :: forall result.
    (Client -> Async Error result) -> Pool -> Async Error result
withTransaction callback pool = fromEitherCont \continue ->
    Pool.withTransaction
        (\callbackCont client -> runAsync (callback client) callbackCont)
        continue
        pool
