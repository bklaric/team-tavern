module TeamTavern.Server.Session.Start.CheckDiscord (checkDiscord) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Data.Variant (inj)
import Jarilo (badRequest_)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordUserContent, discordEmail)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (Confirmation, addConfirmation)
import Type.Proxy (Proxy(..))

-- The Discord address fills a missing email and never replaces one, since the
-- player may have chosen another.
queryString :: Query
queryString = Query """
    with found as (
        select id, nickname, email
        from player
        where discord_id = $1
        for update
    ),
    filled as (
        update player
        set email = $2::text, email_confirmed = $3::boolean
        from found
        where player.id = found.id
            and found.email is null
            and $2::text is not null
        returning player.id
    )
    select found.id, found.nickname, exists (select 1 from filled) as filled
    from found
    """

checkDiscord :: ∀ querier. Querier querier =>
    querier -> DiscordUserContent
    -> Async _ {id :: Int, nickname :: String, confirmation :: Maybe Confirmation}
checkDiscord querier discordUser = do
    let email = discordEmail discordUser
    {id, nickname, filled} :: {id :: Int, nickname :: String, filled :: Boolean} <-
        queryFirst
            (badRequest_ $ inj (Proxy :: _ "unknownDiscord") {nickname: discordUser.username})
            querier queryString
            (discordUser.id : toNullable (email <#> _.email) :| maybe false _.confirmed email)
    confirmation <- case email of
        Just {email: email', confirmed: false} | filled -> do
            nonce <- addConfirmation querier id email'
            pure $ Just {email: email', nickname, nonce}
        _ -> pure Nothing
    pure {id, nickname, confirmation}
