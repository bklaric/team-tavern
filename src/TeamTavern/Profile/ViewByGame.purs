module TeamTavern.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Profile.ViewByGame.LogError (logError)
import TeamTavern.Profile.ViewByGame.SendResponse (sendResponse)

viewByGame :: forall left. Pool -> Handle -> Async left Response
viewByGame pool handle =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool handle
