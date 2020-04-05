module TeamTavern.Server.Profile.ViewGamePlayers where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Filters, Handle, ProfilePage, Timezone)
import TeamTavern.Server.Profile.ViewGamePlayers.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewGamePlayers.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewGamePlayers.LogError (logError)
import TeamTavern.Server.Profile.ViewGamePlayers.SendResponse (sendResponse)

viewGamePlayers
    :: forall left
    .  Pool
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async left Response
viewGamePlayers pool handle page timezone filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle timezone filters

            pure { profiles, count }
