module TeamTavern.Server.Player.Register.AddPlayerDiscord (addPlayerDiscord) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Variant (inj)
import Jarilo (badRequest_, internal__)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import Postgres.Result (rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (read)

type DiscordUserContent =
    { id :: String
    , username :: String
    , discriminator :: String
    }

queryString :: Query
queryString = Query """
    insert into player (nickname, discord_id, discord_tag)
    values ($1, $2, $3)
    returning id
    """

addPlayerDiscord :: forall querier. Querier querier =>
    querier -> Nickname -> DiscordUserContent -> Async _ Int
addPlayerDiscord querier nickname {id, username, discriminator} = do
    result <- querier # query queryString
        (nickname : id :| (username <> "#" <> discriminator))
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
