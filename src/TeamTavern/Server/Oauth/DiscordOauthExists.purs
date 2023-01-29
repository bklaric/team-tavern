module TeamTavern.Server.Oauth.DiscordOauthExists where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Jarilo (ok, ok_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Routes.Oauth.DiscordOauthExists as DiscordOauthExists
import TeamTavern.Server.Infrastructure.Cookie (setCookieHeaderFull)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Oauth.Shared.FetchDiscordUser (fetchDiscordUser)
import TeamTavern.Server.Player.Domain.Id (Id(..))
import TeamTavern.Server.Player.Domain.Nickname (Nickname(..))
import TeamTavern.Server.Session.Domain.Token as Token
import TeamTavern.Server.Session.Start.CreateSession (createSession)

type DiscordUserContent =
    { id :: String
    , username :: String
    , discriminator :: String
    }

queryString :: Query
queryString = Query """
    select id, nickname
    from player
    where discord_id = $1
    """

checkExists :: forall querier. Querier querier =>
    querier -> DiscordUserContent -> Async _ (Maybe {id :: Int, nickname :: String})
checkExists querier {id} =
    queryFirstMaybe querier queryString (id : [])

discordOauthExists :: ∀ left.
    Deployment -> Pool -> DiscordOauthExists.RequestContent -> Async left _
discordOauthExists deployment pool content =
    sendResponse "Error checking if Discord player exists" do
    -- Fetch user info from Discord.
    discordUser <- fetchDiscordUser content.accessToken
    pool # transaction \client -> do
        -- Check if we already have this player in our database.
        playerMaybe <- checkExists client discordUser
        case playerMaybe of
            -- If he exists, sign him in.
            Just {id, nickname} -> do
                token <- Token.generate
                createSession id token client
                let cookies = {id: Id id, nickname: Nickname nickname, token}
                pure $ ok (setCookieHeaderFull deployment cookies) {exists: true}
            -- Else just return false.
            Nothing -> pure $ ok_ {exists: false}
