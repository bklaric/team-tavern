module TeamTavern.Server.Profile.ViewPlayerProfilesByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Profile.Shared (ProfilePage)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Types (Timezone, Handle)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.SendResponse (sendResponse)
import Type.Proxy (Proxy(..))

viewPlayerProfilesByGame
    :: forall left
    .  Pool
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async left Response
viewPlayerProfilesByGame pool handle page timezone filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (Proxy :: _ "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle timezone filters

            pure { profiles, count }
