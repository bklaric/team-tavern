module TeamTavern.Server.Profile.Infrastructure.PatchPlayerContacts (patchPlayerContacts) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError_)
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
        ea_id = coalesce($6, ea_id),
        ubisoft_username = coalesce($7, ubisoft_username),
        psn_id = coalesce($8, psn_id),
        gamer_tag = coalesce($9, gamer_tag),
        friend_code = coalesce($10, friend_code)
        where player.id = $1
    """

queryParameters :: Int -> Contacts -> Array QueryParameter
queryParameters playerId contacts =
    playerId
    : toNullable contacts.discordTag
    : toNullable contacts.steamId
    : toNullable contacts.riotId
    : toNullable contacts.battleTag
    : toNullable contacts.eaId
    : toNullable contacts.ubisoftUsername
    : toNullable contacts.psnId
    : toNullable contacts.gamerTag
    :| toNullable contacts.friendCode

patchPlayerContacts :: forall querier errors. Querier querier =>
    querier -> Int -> Contacts -> Async (InternalError_ errors) Unit
patchPlayerContacts querier playerId contacts =
    queryNone querier queryString (queryParameters playerId contacts)
