module TeamTavern.Server.Account.SwitchToDiscord (switchToDiscord) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, NotAuthorizedRow_, OkRow, TooManyRequestsRow, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Account.SwitchToDiscord as SwitchToDiscord
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Account.Infrastructure.SwitchSignIn (askProvider, switchSignIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.DiscordSignIn (DiscordClient, exchangeDiscordCode)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

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
    Limiter -> DiscordClient -> Pool -> Cookies -> String -> SwitchToDiscord.RequestContent
    -> Async left (Variant (OkRow SwitchToDiscord.OkContent + BadRequestRow SwitchToDiscord.BadContent + NotAuthorizedRow_ + TooManyRequestsRow Wait + InternalRow_ + ()))
switchToDiscord limit discordClient pool cookies origin { code } =
    sendResponse "Error switching to Discord" do
    { id, token } <- ensureSignedIn pool cookies
    { discordId, tag } <- exchangeDiscordCode discordClient origin code # askProvider limit (unwrap id)
    content :: SwitchToDiscord.OkContent <-
        switchSignIn pool (unwrap id) token queryString (unwrap id : discordId :| tag)
            { constraint: "player_discord_id_key", taken: inj (Proxy :: _ "discordTaken") {} }
    pure $ ok_ content
