module Architecture.Postgres.Pool where

import Prelude

import Async (Async, fromEffect)
import Data.Options (Options)
import Postgres.Client.Config (ClientConfig)
import Postgres.Pool (Pool)
import Postgres.Pool as Pool
import Postgres.Pool.Config (PoolConfig)

create :: forall left.
    Options PoolConfig -> Options ClientConfig -> Async left Pool
create poolConfig clientConfig =
    Pool.create poolConfig clientConfig # fromEffect
