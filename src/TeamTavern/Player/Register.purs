module TeamTavern.Player.Register where

import Prelude

import Data.Validation.Semigroup (invalid, unV)
import Data.Variant (Variant)
import Node.Errors (Error)
import Postgres.Query (class Querier)
import TeamTavern.Architecture (ContV)
import TeamTavern.Player.Email (EmailErrors)
import TeamTavern.Player.Nickname (NicknameErrors)
import TeamTavern.Player.Register.Database (addPlayer)
import TeamTavern.Player.Register.PlayerToRegister (PlayerToRegisterModel, PlayerToRegister)
import TeamTavern.Player.Register.PlayerToRegister as PlayerToRegister

type RegisterPlayerErrors errors = Array (Variant
    ( email :: EmailErrors
    , nickname :: NicknameErrors
    , token :: Error
    , database :: Error
    | errors))

register
    :: forall fields querier errors
    .  Querier querier
    => querier
    -> PlayerToRegisterModel fields
    -> ContV (RegisterPlayerErrors errors) PlayerToRegister
register querier playerToRegisterModel = do
    PlayerToRegister.create playerToRegisterModel
    >>= unV (id >>> invalid >>> pure) (addPlayer querier)
