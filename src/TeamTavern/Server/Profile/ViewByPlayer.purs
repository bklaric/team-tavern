module TeamTavern.Server.Profile.ViewByPlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.ViewByPlayer.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewByPlayer.LogError (logError)
import TeamTavern.Server.Profile.ViewByPlayer.SendResponse (sendResponse)

viewByPlayer :: forall left. Pool -> Nickname -> Async left Response
viewByPlayer pool nickname =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool nickname
