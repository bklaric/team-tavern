module TeamTavern.Profile.ViewByGame where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Infrastructure.ReadHandle (readHandle)
import TeamTavern.Profile.ViewByGame.LoadProfiles (loadProfiles)
import TeamTavern.Profile.ViewByGame.LogError (logError) as ViewByGame
import TeamTavern.Profile.ViewByGame.Response (response) as ViewByGame

viewByGame :: forall left. Pool -> String -> Async left Response
viewByGame pool handle' =
    ViewByGame.response $ examineLeftWithEffect ViewByGame.logError do
    -- Read handle from route.
    handle <- readHandle handle'

    -- Load profiles.
    loadProfiles pool handle
