module TeamTavern.Server.Profile.Infrastructure.PatchTeamContacts (patchTeamContacts) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
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
        ea_id = coalesce($7, ea_id),
        ubisoft_username = coalesce($8, ubisoft_username),
        psn_id = coalesce($9, psn_id),
        gamer_tag = coalesce($10, gamer_tag),
        friend_code = coalesce($11, friend_code)
        where team.handle = $1
    """

queryParameters :: String -> Contacts -> Array QueryParameter
queryParameters teamHandle contacts =
    teamHandle
    : toNullable contacts.discordTag
    : toNullable contacts.discordServer
    : toNullable contacts.steamId
    : toNullable contacts.riotId
    : toNullable contacts.battleTag
    : toNullable contacts.eaId
    : toNullable contacts.ubisoftUsername
    : toNullable contacts.psnId
    : toNullable contacts.gamerTag
    :| toNullable contacts.friendCode

patchTeamContacts :: ∀ querier errors. Querier querier =>
    querier -> String -> Contacts -> Async (InternalTerror_ errors) Unit
patchTeamContacts querier teamHandle contacts =
    queryNone querier queryString (queryParameters teamHandle contacts)
