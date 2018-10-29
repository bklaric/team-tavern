module TeamTavern.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Infrastructure.ReadHandle (readHandle)
import TeamTavern.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Profile.ViewByGame.LogError (logError)
import TeamTavern.Profile.ViewByGame.Response (response)

viewByGame :: forall left. Pool -> String -> Async left Response
viewByGame pool handle' =
    response $ examineLeftWithEffect logError do
    -- Read handle from route.
    handle <- readHandle handle'

    -- Load profiles.
    loadProfiles pool handle
