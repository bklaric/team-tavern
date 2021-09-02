module TeamTavern.Server.Team.Infrastructure.WriteContacts (writeContacts) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (Contacts)

-- Write contacts
queryString :: Query
queryString = Query """
    update team
    set
        discord_tag = $2,
        discord_server = $3,
        steam_id = $4,
        riot_id = $5,
        battle_tag = $6,
        psn_id = $7,
        gamer_tag = $8,
        friend_code = $9
    where team.id = $1
    """

queryParameters :: Int -> Contacts -> Array QueryParameter
queryParameters teamId contacts =
    teamId
    : toNullable contacts.discordTag
    : toNullable contacts.discordServer
    : toNullable contacts.steamId
    : toNullable contacts.riotId
    : toNullable contacts.battleTag
    : toNullable contacts.psnId
    : toNullable contacts.gamerTag
    :| toNullable contacts.friendCode

writeContacts :: forall querier errors. Querier querier =>
    querier -> Int -> Contacts -> Async (InternalError errors) Unit
writeContacts querier teamId contacts =
    queryNone querier queryString (queryParameters teamId contacts)
