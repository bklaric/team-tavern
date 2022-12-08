module TeamTavern.Server.Profile.ViewTeamProfilesByGame where

import Prelude

import Async (Async)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Profile.Shared (ProfilePage)
import TeamTavern.Routes.Shared.Filters (Filters)
import TeamTavern.Routes.Shared.Types (Timezone, Handle)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (loadProfiles)

viewTeamProfilesByGame
    :: forall left
    .  Pool
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async left _
viewTeamProfilesByGame pool handle page timezone filters =
    sendResponse "Error viewing team profiles by game" do
    profiles <- pool # transaction
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle timezone filters

            pure { profiles, count }
    pure $ ok_ profiles
