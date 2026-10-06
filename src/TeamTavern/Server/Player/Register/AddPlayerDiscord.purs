module TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord) where

import Prelude

import Async (Async)
import Data.Maybe (maybe)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..))
import Data.Variant (inj)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordUserContent, discordEmail, discordTag)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Infrastructure.InsertPlayer (InsertPlayerError, insertPlayer)
import Type.Proxy (Proxy(..))

-- Email uniqueness holds among password players only, so a Discord player's
-- address never collides.
queryString :: Query
queryString = Query """
    insert into player (nickname, discord_id, email, email_confirmed, discord_tag)
    values ($1, $2, $3, $4, $5)
    returning id
    """

addPlayerDiscord :: ∀ querier other errors. Querier querier =>
    querier -> Nickname -> DiscordUserContent
    -> Async (InsertPlayerError (discordTaken :: {} | other) errors) Int
addPlayerDiscord querier nickname discordUser = do
    let email = discordEmail discordUser
    insertPlayer [ Tuple "player_discord_id_key" $ inj (Proxy :: _ "discordTaken") {} ]
        querier queryString
        ( nickname
        : discordUser.id
        : toNullable (email <#> _.email)
        : maybe false _.confirmed email
        :| discordTag discordUser
        )
