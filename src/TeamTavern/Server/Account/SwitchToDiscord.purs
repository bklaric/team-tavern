module TeamTavern.Server.Account.SwitchToDiscord (switchToDiscord) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (inj)
import Jarilo (badRequest_, internal__, ok_)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Routes.Account.SwitchToDiscord as SwitchToDiscord
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl, discordTag, fetchDiscordUser)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (read)

-- Discord takes the password's place, and the email stays. Its username
-- becomes the Discord contact only where the account's posts offer none, since
-- the player may have given another.
queryString :: Query
queryString = Query """
    with held as (
        select discord_tag from player where id = $1 for update
    )
    update player
    set discord_id = $2, password_hash = null, discord_tag = coalesce(held.discord_tag, $3)
    from held
    where player.id = $1
    returning case when held.discord_tag is null then player.discord_tag end as contact
    """

switchToDiscord :: ∀ left.
    DiscordApiUrl -> Pool -> Cookies -> SwitchToDiscord.RequestContent -> Async left _
switchToDiscord discordApiUrl pool cookies { accessToken } =
    sendResponse "Error switching to Discord" do
    { id } <- ensureSignedIn pool cookies
    discordUser <- fetchDiscordUser discordApiUrl accessToken
    result <- pool # query queryString (unwrap id : discordUser.id :| discordTag discordUser)
        # lmap \error -> case code error == unique_violation, constraint error of
            true, Just "player_discord_id_key" -> Terror
                (badRequest_ $ inj (Proxy :: _ "discordTaken") {})
                [ "Another account signs in with Discord: " <> discordUser.id, print error ]
            _, _ -> Terror internal__ $ databaseErrorLines error
    row <- result # rows # head # note (Terror internal__ [ "Expected the switched player, got no rows." ])
    content :: SwitchToDiscord.OkContent <- read row
        # lmap \error -> Terror internal__ [ "Error reading the Discord contact: " <> show error ]
    pure $ ok_ content
