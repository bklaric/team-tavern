module TeamTavern.Player.Register.Database (addPlayer) where

import Prelude

import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Validation.Semigroup (invalid, valid)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter(..))
import TeamTavern.Architecture (ContV)
import TeamTavern.Database (query)
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
    -> ContV (Array (Variant (database :: Error | errors))) PlayerToRegister
addPlayer querier playerToRegister = do
    query insertPlayerQuery (insertPlayerParameters playerToRegister) querier
    <#>
    case _ of
    Left error -> invalid $ singleton $ inj (SProxy :: SProxy "database") error
    Right _ -> valid playerToRegister
