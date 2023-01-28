module TeamTavern.Server.Oauth.DiscordOauth where

import Prelude

import Async (Async, attempt, left, note)
import Browser.Async.Fetch (fetch, method)
import Browser.Async.Fetch.Response (text)
import Browser.Fetch (headers)
import Browser.Fetch.Response (status)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Variant (inj)
import Error.Class (message, name)
import Jarilo (badRequest_, internal__, noContent)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import Postgres.Result (rows)
import TeamTavern.Routes.Oauth.DiscordOauth as DiscordOauth
import TeamTavern.Server.Infrastructure.Cookie (setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Domain.Nickname (Nickname, validateNickname')
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (read, readJSON)

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

addPlayer :: forall querier. Querier querier =>
    querier -> Nickname -> DiscordUserContent -> Async _ Int
addPlayer querier nickname {id, username, discriminator} = do
    result <- querier # query queryString
        (nickname : id :| (username <> "#" <> discriminator))
        # lmap \error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key"
                || constraint error == Just "player_lower_nickname_key"
                -> Terror
                    (badRequest_ $ inj (Proxy :: _ "nicknameTaken") {})
                    ["Player nickname is taken: " <> show nickname, print error]
            _ -> Terror internal__ $ databaseErrorLines error
    row <- result # rows # head # note (Terror internal__
        ["Expected player id in query result, got no rows."])
    row # (read :: _ -> _ _ { id :: Int })
        <#> _.id
        # lmap (\error -> Terror internal__ ["Error reading player id: " <> show error])

discordOauth :: ∀ left.
    Deployment -> Pool -> DiscordOauth.RequestContent -> Async left _
discordOauth deployment pool content =
    sendResponse "Error registering player with Discord" do
    nickname <- validateNickname' content.nickname
    let userUrl = "https://discord.com/api/users/@me"
    let options = method := GET <> headers := {"Authorization": "Bearer " <> content.accessToken}
    result <- fetch userUrl options # attempt
    case result of
        Left error -> left $ Terror internal__
            ["Error fetching user data from Discord: " <> name error <> " " <> message error]
        Right response ->
            if status response == 200
            then do
                discordUser :: DiscordUserContent <- text response >>= readJSON
                    # lmap \error' -> Terror internal__
                        ["Error parsing Discord user content: " <> show error']
                token <- Token.generate
                id <- pool # transaction \client -> do
                    id <- addPlayer client nickname discordUser
                    createSession id token client
                    pure id
                pure $ noContent $ setCookieHeaderFull deployment {id: Id id, nickname, token}
            else do
                text' <- text response
                left $ Terror internal__
                    [ "Got unexpected response from Discord user endpoint: " <> (show $ status response)
                    , "With following content: " <> text'
                    ]
