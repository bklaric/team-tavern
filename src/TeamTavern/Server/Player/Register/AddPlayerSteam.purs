module TeamTavern.Server.Player.Register.AddPlayerSteam (addPlayerSteam) where

import Prelude

import Async (Async)
import Data.Tuple (Tuple(..))
import Data.Variant (inj)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.ValidateEmail (Email)
import TeamTavern.Server.Infrastructure.ValidateEmail as Email
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Infrastructure.InsertPlayer (InsertPlayerError, insertPlayer)
import Type.Proxy (Proxy(..))

-- The SteamID64 is the Steam contact too. Email uniqueness holds among
-- password players only, so a Steam player's address never collides.
queryString :: Query
queryString = Query """
    insert into player (nickname, email, steam_sign_in_id, steam_id)
    values ($1, $2, $3, $3)
    returning id
    """

addPlayerSteam :: ∀ querier other errors. Querier querier =>
    querier -> { nickname :: Nickname, email :: Email, steamId :: String }
    -> Async (InsertPlayerError (steamTaken :: {} | other) errors) Int
addPlayerSteam querier { nickname, email, steamId } =
    insertPlayer [ Tuple "player_steam_sign_in_id_key" $ inj (Proxy :: _ "steamTaken") {} ]
        querier queryString (nickname : Email.toString email :| steamId)
