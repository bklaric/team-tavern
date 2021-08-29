module TeamTavern.Server.Profile.Infrastructure.PatchContacts (patchContacts) where

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
        discord_tag = coalesce($2, discord_tag),
        steam_id = coalesce($3, steam_id),
        riot_id = coalesce($4, riot_id),
        battle_tag = coalesce($5, battle_tag),
        psn_id = coalesce($6, psn_id),
        gamer_tag = coalesce($7, gamer_tag),
        friend_code = coalesce($8, friend_code)
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

patchContacts :: forall querier errors. Querier querier =>
    querier -> Int -> Contacts -> Async (InternalError errors) Unit
patchContacts querier playerId contacts =
    queryNone querier queryString (queryParameters playerId contacts)
