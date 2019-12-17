module TeamTavern.Server.Password.Forgot.AddPasswordReset
    (Nickname, Player, AddPasswordResetError, addPasswordReset) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:|))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Password.Forgot.ReadEmailAddress (Email)
import TeamTavern.Server.Player.Domain.Nonce (Nonce)

type Nickname = String

type Player = { email :: Email, nickname :: Nickname }

type AddPasswordResetError errors = Variant
  ( databaseError :: Error
  , notFound :: Email
  , unreadablePlayer :: { player :: Foreign, errors :: MultipleErrors }
  | errors )

queryString :: Query
queryString = Query """
    with inserted as (
        insert into password_reset (player_id, nonce)
        select player.id, $2
        from player
        where lower(player.email) = lower($1)
        returning player_id
    )
    select player.email, player.nickname
    from player
        join inserted on inserted.player_id = player.id
    """

addPasswordReset
    :: forall errors
    .  Pool
    -> Email
    -> Nonce
    -> Async (AddPasswordResetError errors) Player
addPasswordReset pool email nonce = do
    result <- pool
        # query queryString (email :| nonce)
        # label (SProxy :: SProxy "databaseError")
    player <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "notFound") email)
    player' <- read player
        # labelMap (SProxy :: SProxy "unreadablePlayer") { player, errors: _ }
    pure player'
