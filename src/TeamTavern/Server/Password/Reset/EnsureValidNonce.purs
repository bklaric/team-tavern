module TeamTavern.Server.Password.Reset.EnsureValidNonce
    (PlayerId, EnsureValidNonceError, ensureValidNonce) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), (:))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Password.Reset.ReadNewPassword (Nonce)

type PlayerId = Int

type EnsureValidNonceError errors = Variant
    ( databaseError :: Error
    , invalidNonce :: Nonce
    , unreadablePlayer :: { player :: Foreign, errors :: MultipleErrors }
    | errors )

queryString :: Query
queryString = Query """
    update password_reset
    set consumed = true
    where password_reset.nonce = $1
    and password_reset.consumed = false
    and extract(epoch from (now() - password_reset.created)) < 3600 -- 1 hour
    returning password_reset.player_id as "playerId"
    """

ensureValidNonce :: forall querier errors. Querier querier =>
    querier -> Nonce -> Async (EnsureValidNonceError errors) PlayerId
ensureValidNonce querier nonce = do
    result <- querier
        # query queryString (nonce : [])
        # label (SProxy :: SProxy "databaseError")
    player <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "invalidNonce") nonce)
    { playerId } :: { playerId :: PlayerId } <- read player
        # labelMap (SProxy :: SProxy "unreadablePlayer") { player, errors: _ }
    pure playerId
