module TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Data.Variant (inj)
import Jarilo (badRequest_, internal__)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordUserContent, discordEmail, discordTag)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (read)

-- Email uniqueness holds among password players only, so a Discord player's
-- address never collides.
queryString :: Query
queryString = Query """
    insert into player (nickname, discord_id, email, email_confirmed, discord_tag)
    values ($1, $2, $3, $4, $5)
    returning id
    """

addPlayerDiscord :: ∀ querier. Querier querier =>
    querier -> Nickname -> DiscordUserContent -> Async _ Int
addPlayerDiscord querier nickname discordUser = do
    let email = discordEmail discordUser
    result <- querier
        # query queryString
            ( nickname
            : discordUser.id
            : toNullable (email <#> _.email)
            : maybe false _.confirmed email
            :| discordTag discordUser
            )
        # lmap \error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key"
                || constraint error == Just "player_lower_nickname_key"
                -> Terror
                    (badRequest_ $ inj (Proxy :: _ "nicknameTaken") {})
                    ["Player nickname is taken: " <> show nickname, print error]
            true | constraint error == Just "player_discord_id_key"
                -> Terror
                    (badRequest_ $ inj (Proxy :: _ "discordTaken") {})
                    ["Discord account is already associated to an account"]
            _ -> Terror internal__ $ databaseErrorLines error
    row <- result # rows # head # note (Terror internal__
        ["Expected player id in query result, got no rows."])
    row # (read :: _ -> _ _ { id :: Int })
        <#> _.id
        # lmap (\error -> Terror internal__ ["Error reading player id: " <> show error])
