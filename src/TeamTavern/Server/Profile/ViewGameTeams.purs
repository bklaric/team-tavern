module TeamTavern.Server.Profile.ViewGameTeams where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Filters, Handle, ProfilePage, Timezone)
import TeamTavern.Server.Profile.ViewGameTeams.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewGameTeams.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewGameTeams.LogError (logError)
import TeamTavern.Server.Profile.ViewGameTeams.SendResponse (sendResponse)

viewGameTeams
    :: forall left
    .  Pool
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async left Response
viewGameTeams pool handle page timezone filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle timezone filters

            pure { profiles, count }
