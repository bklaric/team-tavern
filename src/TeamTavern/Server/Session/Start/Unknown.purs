-- | What a sign-in with Discord, Steam or Google answers for an account new to
-- | the site, which the nickname prompt then registers. Each provider answers
-- | a sign-in only once, so the answer comes with a ticket to register with,
-- | added outside the session's transaction, whose failing would take it back.
module TeamTavern.Server.Session.Start.Unknown (unknownDiscord, unknownGoogle, unknownSteam) where

import Prelude

import Async (Async, left)
import Control.Parallel (parallel, sequential)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, badRequest_)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Server.Infrastructure.DiscordSignIn (DiscordUser)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.FetchSteamNickname (fetchSteamNickname)
import TeamTavern.Server.Infrastructure.GoogleSignIn (GoogleUser)
import TeamTavern.Server.Infrastructure.ResolveSteamId (SteamApi)
import TeamTavern.Server.Player.Domain.Provider (Provider(..))
import TeamTavern.Server.Player.Infrastructure.SignInTicket (addSignInTicket)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | A ticket to register with and the nickname to offer.
type Unknown = { ticket :: String, nickname :: String }

-- | A ticket, which carries the address Discord gave and the account's tag,
-- | and the Discord username to offer as a nickname.
unknownDiscord :: ∀ a other errors. Pool -> DiscordUser ->
    Async (TerrorVar (InternalRow_ + BadRequestRow (Variant (unknownDiscord :: Unknown | other)) + errors)) a
unknownDiscord pool { discordId, email, username, tag } = do
    ticket <- addSignInTicket pool Discord { providerId: discordId, email, discordTag: Just tag }
    left $ Terror
        (badRequest_ $ inj (Proxy :: _ "unknownDiscord") { ticket, nickname: username })
        [ "No account signs in with Discord: " <> discordId ]

-- | A ticket and the Steam profile name to offer as a nickname.
unknownSteam :: ∀ a other errors. SteamApi -> Pool -> String ->
    Async (TerrorVar (InternalRow_ + BadRequestRow (Variant (unknownSteam :: Unknown | other)) + errors)) a
unknownSteam steamApi pool steamId = do
    unknown <- sequential $ { ticket: _, nickname: _ }
        <$> parallel (addSignInTicket pool Steam { providerId: steamId, email: Nothing, discordTag: Nothing })
        <*> parallel (fetchSteamNickname steamApi steamId)
    left $ Terror
        (badRequest_ $ inj (Proxy :: _ "unknownSteam") unknown)
        [ "No account signs in with Steam: " <> steamId ]

-- | A ticket, which carries the address Google gave, and the Google name to
-- | offer as a nickname.
unknownGoogle :: ∀ a other errors. Pool -> GoogleUser ->
    Async (TerrorVar (InternalRow_ + BadRequestRow (Variant (unknownGoogle :: Unknown | other)) + errors)) a
unknownGoogle pool { googleId, email, nickname } = do
    ticket <- addSignInTicket pool Google { providerId: googleId, email, discordTag: Nothing }
    left $ Terror
        (badRequest_ $ inj (Proxy :: _ "unknownGoogle") { ticket, nickname })
        [ "No account signs in with Google: " <> googleId ]
