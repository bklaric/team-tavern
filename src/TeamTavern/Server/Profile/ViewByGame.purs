module TeamTavern.Server.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Filters, Handle, ProfileIlk, ProfilePage, Timezone)
import TeamTavern.Server.Profile.ViewByGame.LoadProfileCount (loadProfileCount)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByGame.LogError (logError)
import TeamTavern.Server.Profile.ViewByGame.SendResponse (sendResponse)

viewByGame
    :: forall left
    .  Pool
    -> Handle
    -> ProfileIlk
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async left Response
viewByGame pool handle ilk page timezone filters =
    sendResponse $ examineLeftWithEffect logError do
    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load profiles.
            profiles <- loadProfiles client handle ilk page timezone filters

            -- Load profile count.
            count <- loadProfileCount client handle ilk timezone filters

            pure { profiles, count }
