module TeamTavern.Server.Profile.ViewPlayerProfilesByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Timezone (Timezone)
import TeamTavern.Server.Profile.Routes (Handle, ProfilePage)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.SendResponse (sendResponse)

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
    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle timezone filters

            pure { profiles, count }
