module TeamTavern.Server.Profile.Infrastructure.PatchTeamContacts (patchTeamContacts) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (Contacts)

queryString :: Query
queryString = Query """
    update team
    set
        discord_tag = coalesce($2, discord_tag),
        discord_server = coalesce($3, discord_server),
        steam_id = coalesce($4, steam_id),
        riot_id = coalesce($5, riot_id),
        battle_tag = coalesce($6, battle_tag),
        psn_id = coalesce($7, psn_id),
        gamer_tag = coalesce($8, gamer_tag),
        friend_code = coalesce($9, friend_code)
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

patchTeamContacts :: forall querier errors. Querier querier =>
    querier -> Int -> Contacts -> Async (InternalError errors) Unit
patchTeamContacts querier teamId contacts =
    queryNone querier queryString (queryParameters teamId contacts)
