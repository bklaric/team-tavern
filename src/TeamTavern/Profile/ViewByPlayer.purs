module TeamTavern.Profile.ViewByPlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Profile.ViewByPlayer.LoadProfiles (loadProfiles)
import TeamTavern.Profile.ViewByPlayer.LogError (logError)
import TeamTavern.Profile.ViewByPlayer.SendResponse (sendResponse)

viewByPlayer :: forall left. Pool -> Nickname -> Async left Response
viewByPlayer pool nickname =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool nickname
