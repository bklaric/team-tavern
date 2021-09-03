module TeamTavern.Server.Player.UpdateContacts.WriteContacts (writeContacts) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (Contacts)

queryString :: Query
queryString = Query """
    update player
    set
        discord_tag = $2,
        steam_id = $3,
        riot_id = $4,
        battle_tag = $5,
        psn_id = $6,
        gamer_tag = $7,
        friend_code = $8
    where player.id = $1
    """

queryParameters :: Int -> Contacts -> Array QueryParameter
queryParameters playerId contacts =
    playerId
    : toNullable contacts.discordTag
    : toNullable contacts.steamId
    : toNullable contacts.riotId
    : toNullable contacts.battleTag
    : toNullable contacts.psnId
    : toNullable contacts.gamerTag
    :| toNullable contacts.friendCode

writeContacts :: forall querier errors. Querier querier =>
    querier -> Int -> Contacts -> Async (InternalError errors) Unit
writeContacts querier playerId contacts =
    queryNone querier queryString (queryParameters playerId contacts)
