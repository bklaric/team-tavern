module TeamTavern.Server.Player.Register.AddPlayerGoogle (addPlayerGoogle) where

import Prelude

import Async (Async)
import Data.Maybe (maybe)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..))
import Data.Variant (inj)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Infrastructure.InsertPlayer (InsertPlayerError, insertPlayer)
import TeamTavern.Server.Player.Infrastructure.SignInTicket (Ticketed)
import Type.Proxy (Proxy(..))

-- Email uniqueness holds among password players only, so a Google player's
-- address never collides.
queryString :: Query
queryString = Query """
    insert into player (nickname, google_id, email, email_confirmed)
    values ($1, $2, $3, $4)
    returning id
    """

addPlayerGoogle :: ∀ querier other errors. Querier querier =>
    querier -> Nickname -> Ticketed
    -> Async (InsertPlayerError (googleTaken :: {} | other) errors) Int
addPlayerGoogle querier nickname { providerId: googleId, email } =
    insertPlayer [ Tuple "player_google_id_key" $ inj (Proxy :: _ "googleTaken") {} ]
        querier queryString
        ( nickname
        : googleId
        : toNullable (email <#> _.email)
        :| maybe false _.confirmed email
        )
