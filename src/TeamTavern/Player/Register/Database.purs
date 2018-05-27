module TeamTavern.Player.Register.Database (addPlayer) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(SProxy), Variant)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter(..))
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Register.PlayerToRegister (PlayerToRegister)

insertPlayerQuery :: Query
insertPlayerQuery =
    Query "insert into player (email, nickname, token) values ($1, $2, $3)"

insertPlayerParameters :: PlayerToRegister -> Array QueryParameter
insertPlayerParameters { email, nickname, token } =
    [unwrap email, unwrap nickname, unwrap token] <#> QueryParameter

addPlayer
    :: forall errors querier
    .  Querier querier
    => querier
    -> PlayerToRegister
    -> Async (Variant (database :: Error | errors)) PlayerToRegister
addPlayer querier playerToRegister =
    query insertPlayerQuery (insertPlayerParameters playerToRegister) querier
    # label (SProxy :: SProxy "database")
    # map (const playerToRegister)
