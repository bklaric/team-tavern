module TeamTavern.Server.Session.Start.CheckDiscord (checkDiscord) where

import Prelude

import Async (Async)
import Data.Variant (inj)
import Jarilo (badRequest_)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Postgres (queryFirst)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordUserContent)
import Type.Proxy (Proxy(..))

queryString :: Query
queryString = Query """
    select id, nickname
    from player
    where discord_id = $1
    """

checkDiscord :: forall querier. Querier querier =>
    querier -> DiscordUserContent -> Async _ {id :: Int, nickname :: String}
checkDiscord querier {id} =
    queryFirst (badRequest_ $ inj (Proxy :: _ "unknownDiscord") {}) querier queryString (id : [])
