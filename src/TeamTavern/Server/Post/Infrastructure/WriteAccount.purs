module TeamTavern.Server.Post.Infrastructure.WriteAccount (writeAccount) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import JavaScript.Npm.Pg.Client (Client)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Post (AccountContent)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Yoga.JSON (writeImpl)

-- $6 is { "<contact kind>": "<account>" }.
queryString :: Query
queryString = Query """
    update player
    set country = coalesce($2::text, country),
        languages = case when cardinality($3::text[]) > 0 then $3::text[] else languages end,
        birthday = coalesce($4::date, birthday),
        timezone = coalesce($5::text, timezone),
        discord_tag = coalesce($6::jsonb->>'discord', discord_tag),
        steam_id = coalesce($6::jsonb->>'steam', steam_id),
        riot_id = coalesce($6::jsonb->>'riot', riot_id),
        battle_tag = coalesce($6::jsonb->>'battle_tag', battle_tag),
        ea_id = coalesce($6::jsonb->>'ea', ea_id),
        ubisoft_username = coalesce($6::jsonb->>'ubisoft', ubisoft_username),
        psn_id = coalesce($6::jsonb->>'psn', psn_id),
        gamer_tag = coalesce($6::jsonb->>'gamer_tag', gamer_tag),
        friend_code = coalesce($6::jsonb->>'friend_code', friend_code)
    where id = $1
    """

-- | Writes the facts and contacts the post screen gave, which every post of
-- | the player's shows (brief 6, step 3). One it left empty keeps what the
-- | account holds.
writeAccount :: ∀ errors. Client -> Int -> AccountContent -> Async (InternalTerror_ errors) Unit
writeAccount client playerId account =
    queryNone client queryString
        ( playerId
        : toNullable account.country
        : account.languages
        : toNullable account.birthday
        : toNullable account.timezone
        :| writeImpl account.contacts
        )
