module TeamTavern.Server.Profile.ViewByPlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Nickname, ProfileIlk)
import TeamTavern.Server.Profile.ViewByPlayer.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByPlayer.LogError (logError)
import TeamTavern.Server.Profile.ViewByPlayer.SendResponse (sendResponse)

viewByPlayer :: forall left. Pool -> Nickname -> ProfileIlk -> Async left Response
viewByPlayer pool nickname ilk =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool nickname ilk
