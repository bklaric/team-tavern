module TeamTavern.Server.Account.SwitchToSteam (switchToSteam) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, NotAuthorizedRow_, OkRow, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Routes.Account.SwitchToSteam as SwitchToSteam
import TeamTavern.Server.Account.Infrastructure.SwitchSignIn (switchSignIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.SteamOpenId (SteamOpenIdUrl, verifySteamReturn)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- Steam takes the place of the password, Discord or Google, and the email
-- stays. Its SteamID64 becomes the Steam contact only where the account's posts
-- offer none, since the player may have given another.
queryString :: Query
queryString = Query """
    with held as (
        select steam_id from player where id = $1 for update
    )
    update player
    set steam_sign_in_id = $2, password_hash = null, discord_id = null, google_id = null,
        steam_id = coalesce(held.steam_id, $2)
    from held
    where player.id = $1
    returning case when held.steam_id is null then player.steam_id end as contact
    """

switchToSteam :: ∀ left.
    SteamOpenIdUrl -> Pool -> Cookies -> String -> SwitchToSteam.RequestContent
    -> Async left (Variant (OkRow SwitchToSteam.OkContent + BadRequestRow SwitchToSteam.BadContent + NotAuthorizedRow_ + InternalRow_ + ()))
switchToSteam steamOpenIdUrl pool cookies origin { assertion } =
    sendResponse "Error switching to Steam" do
    { id, token } <- ensureSignedIn pool cookies
    steamId <- verifySteamReturn steamOpenIdUrl origin pool assertion
    content :: SwitchToSteam.OkContent <-
        switchSignIn pool (unwrap id) token queryString (unwrap id :| steamId)
            { constraint: "player_steam_sign_in_id_key", taken: inj (Proxy :: _ "steamTaken") {} }
    pure $ ok_ content
