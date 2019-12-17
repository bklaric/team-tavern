module TeamTavern.Server.Password.Reset.UpdatePassword
    (UpdatePasswordError, updatePassword) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant)
import Postgres.Async.Query (execute)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Password.Reset.EnsureValidNonce (PlayerId)
import TeamTavern.Server.Player.Domain.Hash (Hash)

type UpdatePasswordError errors = Variant
    ( databaseError :: Error
    | errors )

queryString :: Query
queryString = Query """
    update player
    set password_hash = $2
    where player.id = $1
    """

updatePassword :: forall querier errors. Querier querier =>
    querier -> PlayerId -> Hash -> Async (UpdatePasswordError errors) Unit
updatePassword querier playerId hash =
    querier
        # execute queryString (playerId :| hash)
        # label (SProxy :: SProxy "databaseError")
