module TeamTavern.Server.Account.SwitchToGoogle (switchToGoogle) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, NoContentRow_, NotAuthorizedRow_, TooManyRequestsRow, noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Routes.Account.SwitchToGoogle as SwitchToGoogle
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Account.Infrastructure.SwitchSignIn (askProvider, switchSignIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.GoogleSignIn (GoogleClient, exchangeGoogleCode)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- Google takes the place of the password, Discord or Steam, and the email
-- stays. Google is no contact, so the switch fills none.
queryString :: Query
queryString = Query """
    update player
    set google_id = $2, password_hash = null, discord_id = null, steam_sign_in_id = null
    where id = $1
    returning null::text as contact
    """

switchToGoogle :: ∀ left.
    Limiter -> GoogleClient -> Pool -> Cookies -> String -> SwitchToGoogle.RequestContent
    -> Async left (Variant (NoContentRow_ + BadRequestRow SwitchToGoogle.BadContent + NotAuthorizedRow_ + TooManyRequestsRow Wait + InternalRow_ + ()))
switchToGoogle limit googleClient pool cookies origin { code } =
    sendResponse "Error switching to Google" do
    { id, token } <- ensureSignedIn pool cookies
    { googleId } <- exchangeGoogleCode googleClient origin code # askProvider limit (unwrap id)
    void $ switchSignIn pool (unwrap id) token queryString (unwrap id :| googleId)
        { constraint: "player_google_id_key", taken: inj (Proxy :: _ "googleTaken") {} }
    pure noContent_
