module TeamTavern.Server.Account.SwitchToDiscord (switchToDiscord) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Data.Variant (inj)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Account.SwitchToDiscord as SwitchToDiscord
import TeamTavern.Server.Account.Infrastructure.SwitchSignIn (switchSignIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl, discordTag, fetchDiscordUser)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Proxy (Proxy(..))

-- Discord takes the place of the password, Steam or Google, and the email
-- stays. Its username becomes the Discord contact only where the account's
-- posts offer none, since the player may have given another.
queryString :: Query
queryString = Query """
    with held as (
        select discord_tag from player where id = $1 for update
    )
    update player
    set discord_id = $2, password_hash = null, steam_sign_in_id = null, google_id = null,
        discord_tag = coalesce(held.discord_tag, $3)
    from held
    where player.id = $1
    returning case when held.discord_tag is null then player.discord_tag end as contact
    """

switchToDiscord :: ∀ left.
    DiscordApiUrl -> Pool -> Cookies -> SwitchToDiscord.RequestContent -> Async left _
switchToDiscord discordApiUrl pool cookies { accessToken } =
    sendResponse "Error switching to Discord" do
    { id, token } <- ensureSignedIn pool cookies
    discordUser <- fetchDiscordUser discordApiUrl accessToken
    content :: SwitchToDiscord.OkContent <-
        switchSignIn pool (unwrap id) token queryString (unwrap id : discordUser.id :| discordTag discordUser)
            { constraint: "player_discord_id_key", taken: inj (Proxy :: _ "discordTaken") {} }
    pure $ ok_ content
