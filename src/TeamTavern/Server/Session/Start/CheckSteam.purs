module TeamTavern.Server.Session.Start.CheckSteam (checkSteam) where

import Prelude

import Async (Async, left)
import Control.Parallel (parallel, sequential)
import Data.Maybe (Maybe(..))
import Data.Variant (inj)
import Jarilo (badRequest_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.FetchSteamNickname (fetchSteamNickname)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe)
import TeamTavern.Server.Infrastructure.ResolveSteamId (SteamApi)
import TeamTavern.Server.Player.Infrastructure.SteamTicket (addSteamTicket)
import Type.Proxy (Proxy(..))

queryString :: Query
queryString = Query """
    select id, nickname from player where steam_sign_in_id = $1
    """

-- | The player who signs in with the Steam account. A Steam account new to the
-- | site is answered with a ticket to register it with and the Steam profile
-- | name to offer as a nickname.
checkSteam :: SteamApi -> Pool -> String -> Async _ { id :: Int, nickname :: String }
checkSteam steamApi pool steamId =
    queryFirstMaybe pool queryString (steamId : []) >>= case _ of
        Just player -> pure player
        Nothing -> do
            unknownSteam <- sequential $ { ticket: _, nickname: _ }
                <$> parallel (addSteamTicket pool steamId)
                <*> parallel (fetchSteamNickname steamApi steamId)
            left $ Terror
                (badRequest_ $ inj (Proxy :: _ "unknownSteam") unknownSteam)
                [ "No account signs in with Steam: " <> steamId ]
