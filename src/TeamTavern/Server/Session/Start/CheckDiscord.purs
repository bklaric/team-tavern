module TeamTavern.Server.Session.Start.CheckDiscord (checkDiscord) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Data.Variant (inj)
import Jarilo (badRequest_)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordUserContent, verifiedEmail)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst)
import Type.Proxy (Proxy(..))

-- The verified Discord address fills a missing contact email and never replaces
-- one, since the player may have changed it.
queryString :: Query
queryString = Query """
    update player
    set email = coalesce(email, $2)
    where discord_id = $1
    returning id, nickname
    """

checkDiscord :: forall querier. Querier querier =>
    querier -> DiscordUserContent -> Async _ {id :: Int, nickname :: String}
checkDiscord querier discordUser =
    queryFirst (badRequest_ $ inj (Proxy :: _ "unknownDiscord") {}) querier queryString
        (discordUser.id :| toNullable (verifiedEmail discordUser))
