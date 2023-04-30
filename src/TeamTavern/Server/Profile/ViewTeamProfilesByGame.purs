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
import TeamTavern.Server.Profile.Infrastructure.LoadFieldAndOptionIds (loadFieldAndOptionIds)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (loadProfiles)

viewTeamProfilesByGame
    :: ∀ left
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
            -- Load field and option ids.
            fieldAndOptionIds <- loadFieldAndOptionIds client handle filters.fields

            -- Load profiles and count.
            loadProfiles client handle page timezone filters fieldAndOptionIds

    pure $ ok_ profiles
