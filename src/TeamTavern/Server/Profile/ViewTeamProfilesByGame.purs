module TeamTavern.Server.Profile.ViewTeamProfilesByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Timezone (Timezone)
import TeamTavern.Server.Profile.Routes (Handle, ProfilePage)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.SendResponse (sendResponse)
import Type.Proxy (Proxy(..))

viewTeamProfilesByGame
    :: forall left
    .  Pool
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async left Response
viewTeamProfilesByGame pool handle page timezone filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (Proxy :: _ "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle timezone filters

            pure { profiles, count }
